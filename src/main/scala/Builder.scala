//@
package xyz.hyperreal.docs

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{CopyOption, Files, Path}

import xyz.hyperreal.yaml.read
import xyz.hyperreal.markdown.{Heading, Markdown}
import xyz.hyperreal.backslash.{AST, Command, Parser, Renderer}

import scala.collection.mutable.ArrayBuffer


object Builder {

  val predefinedTopMatterKeys = Set( "layout" )

}

class Builder( src: Path, dst: Path, dryrun: Boolean = false, verbose: Boolean = false ) {

  val srcnorm = src.normalize
  val srcdir = srcnorm.toFile

  require( srcdir.exists, s"source does not exist: $srcdir" )
  require( srcdir.isDirectory, s"source path is not a directory: $srcdir" )
  require( srcdir.canRead, s"source directory is unreadable: $srcdir" )

  val dstnorm = dst.normalize
  val dstdir = dstnorm.toFile
  val layoutdir = srcnorm resolve "_layouts" toFile
  val backslashConfig =
    Map(
      "today" -> "MMMM d, y",
      "include" -> ".",
      "rounding" -> "HALF_EVEN"
    )
  val backslashParser = new Parser( Command.standard )
  val backslashRenderer = new Renderer( backslashParser, backslashConfig )

  require( layoutdir.exists, s"'_layouts directory does not exist: $layoutdir" )
  require( layoutdir.isDirectory, s"not a directory: $layoutdir" )
  require( layoutdir.canRead, s"_layouts directory is unreadable: $layoutdir" )
  verbosely( s"processing layouts: $layoutdir" )

  val layouts = {
    val ls =
      for (l <- layoutdir.listFiles filter (f => f.isFile && f.canRead && f.getName.endsWith(".backslash")))
        yield {
          verbosely( s"reading layout: $l" )

          (withoutExtension(l.getName), backslashParser.parse( io.Source.fromFile(l) ))
        }

    ls toMap
  }

  case class MdFile( dir: Path, file: File, vars: Map[String, Any], md: String, headings: List[Heading], layout: AST )

  val mdFiles = new ArrayBuffer[MdFile]
  val resFiles = new ArrayBuffer[Path]

  def readPhase: Unit = {
    processDirectory( srcnorm, "" )
  }

  def writePhase: Unit = {

    dstdir.mkdirs
    require( dstdir.isDirectory, s"destination path is not a directory: $dstdir" )
    require( dstdir.canWrite, s"destination directory is unwritable: $dstdir" )

    for (MdFile( dir, file, vars, markdown, headings, layout ) <- mdFiles) {
      val dstdir = dir relativize srcnorm resolve dstnorm
      val dstdirfile = dstdir toFile
      val filename = withoutExtension( file.getName )
      val page = backslashRenderer.capture( layout, Map("contents" -> markdown) ++ vars )

      dstdirfile.mkdirs
      require( dstdirfile.exists && dstdirfile.isDirectory, s"failed to create destination directory: $dstdirfile" )
      require( dstdirfile.canWrite, s"destination directory is unwritable: $dstdirfile" )

      val pagepath = dstdir resolve s"$filename.html"

      verbosely( s"writting page: $pagepath" )
      Files.write( pagepath, page.getBytes(StandardCharsets.UTF_8) )
    }

    for (p <- resFiles) {
      val dir = p.getParent
      val dstdir = dir relativize srcnorm resolve dstnorm
      val filename = p.getFileName
      val dstpath = dstdir resolve filename

      verbosely( s"copying asset '$filename' from $dir to $dstdir" )
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
      for (f <- dir.toFile.listFiles) {
        if (f.isDirectory)
          clean( dir resolve f.getName )
        else
          f.delete
      }

      Files delete dir
    }

    if (dstdir.exists)
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

  def processDirectory( parent: Path, sub: String ): Unit = {
    val srcdir = parent resolve sub

    verbosely( s"processing directory: $srcdir" )

    val srcdirfile = srcdir.toFile
    val contents = srcdirfile.listFiles.toList filterNot (_.getName startsWith "_")
    val subdirectories = contents filter (d => d.isDirectory && d.canRead)
    val mds = contents filter (f => f.getName.endsWith(".md") && f.isFile && f.canRead)

    resFiles ++= contents filter (f => !f.getName.endsWith(".md") && f.isFile && f.canRead) map (_.toPath)

    if (mds isEmpty)
      verbosely( "no markdown files" )
    else
      for (f <- mds) {
        verbosely( s"reading markdown file: $f" )

        val filename = withoutExtension( f.getName )
        val s = io.Source.fromFile( f ).mkString
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

        mdFiles += MdFile( srcdir, f, vars, md, headings, layout )
      }

    for (s <- subdirectories)
      processDirectory( srcdir, s.getName )
  }

}