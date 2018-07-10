//@
package xyz.hyperreal.docs

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import xyz.hyperreal.yaml.read
import xyz.hyperreal.markdown.{Heading, Markdown}
import xyz.hyperreal.backslash.{AST, Command, Parser, Renderer}

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._


object Builder {

  val predefinedTopMatterKeys = Set( "layout" )

}

class Builder( src: Path, dst: Path, dryrun: Boolean = false, verbose: Boolean = false ) {

  val srcnorm = src.normalize

  require( Files exists srcnorm, s"source does not exist: $srcnorm" )
  require( Files isDirectory srcnorm, s"source path is not a directory: $srcnorm" )
  require( Files isReadable srcnorm, s"source directory is unreadable: $srcnorm" )

  val dstnorm = dst.normalize
  val layoutdir = srcnorm resolve "_layouts"
  val backslashConfig =
    Map(
      "today" -> "MMMM d, y",
      "include" -> ".",
      "rounding" -> "HALF_EVEN"
    )
  val backslashParser = new Parser( Command.standard )
  val backslashRenderer = new Renderer( backslashParser, backslashConfig )

  require( Files exists layoutdir, s"'_layouts directory does not exist: $layoutdir" )
  require( Files isDirectory layoutdir, s"not a directory: $layoutdir" )
  require( Files isReadable layoutdir, s"_layouts directory is unreadable: $layoutdir" )
  verbosely( s"processing layouts: $layoutdir" )

  val layouts = {
    val templates = (Files list layoutdir).iterator.asScala.toList.filter (p => Files.isRegularFile(p) && Files.isReadable(p) && p.getFileName.toString.endsWith(".backslash"))
    val ls =
      for (l <- templates)
        yield {
          verbosely( s"reading layout: $l" )

          (withoutExtension(l.getFileName.toString), backslashParser.parse( io.Source.fromFile(l.toFile) ))
        }

    ls toMap
  }

  case class MdFile( dir: Path, filename: String, vars: Map[String, Any], md: String, headings: List[Heading], layout: AST )

  val mdFiles = new ArrayBuffer[MdFile]
  val resFiles = new ArrayBuffer[Path]
  val siteHeadings = new ArrayBuffer[(Path, String, Heading)]

  def addHeadings( path: Path, filename: String, headings: List[Heading] ) =
    siteHeadings ++= headings map (h => (path, filename, h))

  def readPhase: Unit = {
    processDirectory( srcnorm, Paths get "" )

    for ((m, i) <- mdFiles zipWithIndex if i > 0) {
      if (m.headings.head.level >= mdFiles(i - 1).headings.last.level)
        addHeadings( m.dir, m.filename, m.headings )
    }
  }

  def writePhase: Unit = {

    Files createDirectories dstnorm
    require( Files isDirectory dstnorm, s"destination path is not a directory: $dstnorm" )
    require( Files isWritable dstnorm, s"destination directory is unwritable: $dstnorm" )

    for (MdFile( dir, filename, vars, markdown, _, layout ) <- mdFiles) {
      val dstdir = dstnorm resolve dir
      val page = backslashRenderer.capture( layout, Map("contents" -> markdown, "page" -> vars) )

      Files createDirectories dstdir
      require( Files.exists(dstdir) && Files.isDirectory(dstdir), s"failed to create destination directory: $dstdir" )
      require( Files isWritable dstdir, s"destination directory is unwritable: $dstdir" )

      val pagepath = dstdir resolve s"$filename.html"

      verbosely( s"writting page: $pagepath" )
      Files.write( pagepath, page.getBytes(StandardCharsets.UTF_8) )
    }

    for (p <- resFiles) {
      val dstpath = dstnorm resolve (srcnorm relativize p)
      val filename = p.getFileName
      val dstdir = dstpath.getParent

      verbosely( s"copying asset '$filename' from ${p.getParent} to $dstdir" )
      Files createDirectories dstdir
      Files.copy( p, dstpath )
    }

  }

  def build: Unit = {
    readPhase

    if (!dryrun) {
      writePhase
    }
  }

  def clean: Unit = {
    def clean( dir: Path ): Unit = {
      for (f <- (Files list dir).iterator.asScala) {
        if (Files isDirectory f)
          clean( dir resolve f.getFileName )
        else
          Files delete f
      }

      Files delete dir
    }

    if (Files exists dstnorm)
      clean( dstnorm )
  }

  def withoutExtension( s: String ) =
    s lastIndexOf '.' match {
      case -1 => s
      case dot => s.substring( 0, dot )
    }

  def verbosely( msg: String ): Unit =
    if (dryrun || verbose)
      println( msg )

  def processDirectory( parent: Path, sub: Path ): Unit = {
    val srcdir = parent resolve sub

    verbosely( s"processing directory: $srcdir" )

    val contents = (Files list srcdir).iterator.asScala.toList filterNot (_.getFileName.toString startsWith "_")
    val subdirectories = contents filter (d => Files.isDirectory(d) && Files.isReadable(d))
    val files = contents filter (f => Files.isRegularFile(f) && Files.isReadable(f))
    val mds = files filter (f => f.getFileName.toString.endsWith(".md"))

    resFiles ++= files filter (f => !f.getFileName.toString.endsWith(".md"))

    if (mds nonEmpty)
      for (f <- mds) {
        verbosely( s"reading markdown file: $f" )

        val filename = withoutExtension( f.getFileName.toString )
        val s = io.Source.fromFile( f.toFile ).mkString
        val (top, src) = {
          val lines = s.lines

          if (lines.hasNext && lines.next == "---") {
            (read( lines takeWhile( _ != "---" ) mkString "\n" ).head match {
              case m: Map[_, _] => m.asInstanceOf[Map[String, Any]]
              case _ => sys.error( s"expected an object as top matter: $f" )
            }, lines mkString "\n")
          } else
            (Map[String, Any](), s)
        }

        val (md, headings) = Markdown.withHeadings( src )
        val layout =
          top get "layout" match {
            case None =>
              if (filename == "index")
                layouts get "index" match {
                  case None =>
                    layouts get "page" match {
                      case None => sys.error( s"neither 'index' nor 'page' layout found for laying out $f" )
                      case Some( l ) => l
                    }
                  case Some( l ) => l
                }
              else
                layouts get "page" match {
                  case None => sys.error( s"'page' layout not found for laying out $f" )
                  case Some( l ) => l
                }
            case Some( l ) =>
              layouts get l.toString match {
                case None => sys.error( s"layout not found: $l in file $f" )
                case Some( ast ) => ast
              }
          }

        val vars = top -- Builder.predefinedTopMatterKeys

        mdFiles += MdFile( srcnorm relativize srcdir, filename, vars, md, headings, layout )
      }

    for (s <- subdirectories)
      processDirectory( srcdir, s.getFileName )
  }

}