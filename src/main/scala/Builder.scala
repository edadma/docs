//@
package xyz.hyperreal.docs

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import scala.collection.mutable.{ArrayBuffer, Buffer, ListBuffer}
import scala.collection.JavaConverters._
import scala.collection.mutable
import xyz.hyperreal.yaml
import xyz.hyperreal.markdown.{Heading, Markdown}
import xyz.hyperreal.backslash.{AST, Command, Parser, Renderer}

import scala.xml.{Elem, Group, Node}


object Builder {

  val predefinedFrontmatterKeys = Set( "layout" )

}

class Builder( src: Path, dst: Path, verbose: Boolean = false ) {

  val srcnorm = src.normalize

  check( Files exists srcnorm, s"source does not exist: $srcnorm" )
  check( Files isDirectory srcnorm, s"source path is not a directory: $srcnorm" )
  check( Files isReadable srcnorm, s"source directory is unreadable: $srcnorm" )

  val dstnorm = dst.normalize
  val backslashConfig =
    Map(
      "today" -> "MMMM d, y",
      "include" -> (srcnorm resolve "_includes").toString,
      "rounding" -> "HALF_EVEN"
    )
  val backslashParser = new Parser( Command.standard )
  val backslashRenderer = new Renderer( backslashParser, backslashConfig )

  val layoutdir = srcnorm resolve "layouts"

  check( Files exists layoutdir, s"'layouts directory does not exist: $layoutdir" )
  check( Files isDirectory layoutdir, s"not a directory: $layoutdir" )
  check( Files isReadable layoutdir, s"layouts directory is unreadable: $layoutdir" )

  val configdir = srcnorm resolve "config"

  check( Files exists configdir, s"'config directory does not exist: $configdir" )
  check( Files isDirectory configdir, s"not a directory: $configdir" )
  check( Files isReadable configdir, s"config directory is unreadable: $configdir" )

  val sources = srcnorm resolve "src"

  check( Files exists sources, s"'sources directory does not exist: $sources" )
  check( Files isDirectory sources, s"not a directory: $sources" )
  check( Files isReadable sources, s"sources directory is unreadable: $sources" )

  info( s"processing layouts: $layoutdir" )

  val layouts = {
    val templates = (Files list layoutdir).iterator.asScala.toList.filter (p => Files.isRegularFile(p) && Files.isReadable(p) && p.getFileName.toString.endsWith(".backslash"))
    val ls =
      for (l <- templates)
        yield {
          info( s"reading layout: $l" )

          (withoutExtension(l.getFileName.toString), backslashParser.parse( io.Source.fromFile(l.toFile) ))
        }

    ls toMap
  }

  info( s"processing configs: $configdir" )

  val configs = {
    val yamls = (Files list configdir).iterator.asScala.toList.filter (p => Files.isRegularFile(p) && Files.isReadable(p) && p.getFileName.toString.endsWith(".yml"))
    val cs =
      for (l <- yamls)
        yield {
          info( s"reading config: $l" )

          (withoutExtension(l.getFileName.toString), yaml.read( io.Source.fromFile(l.toFile) ).head)
        }

    cs toMap
  }
  val tocmin = configs get "toc" match {
    case None => 1
    case Some( m ) => m.asInstanceOf[Map[String, Number]] get "min-level" match {
      case None => 1
      case Some( min: Number ) => min.intValue
    }
  }
  val tocmax = configs("settings").asInstanceOf[Map[String, Any]] get "toc" match {
    case None => 6
    case Some( m ) => m.asInstanceOf[Map[String, Number]] get "max-level" match {
      case None => 6
      case Some( max: Number ) => max.intValue
    }
  }

  case class MdFile( dir: Path, filename: String, vars: Map[String, Any], md: String, headings: List[Heading], layout: AST )

  val mdFiles = new ArrayBuffer[MdFile]
  val resFiles = new ArrayBuffer[Path]
  val navLinks = new ArrayBuffer[Link]
  val pagetocMap = new mutable.HashMap[(Path, String), List[Map[String, Any]]]
  val headingtocMap = new mutable.HashMap[String, List[Map[String, Any]]]

  case class Link( path: String, level: Int, heading: String, id: String, sublinks: Buffer[Link] )

  def readSources: Unit = {
    configs get "pages" match {
      case None => processDirectory( sources )
      case Some( pgs: List[_] ) if pgs.forall(_.isInstanceOf[String]) =>
        for (p <- pgs.asInstanceOf[List[String]]) {
          val folder = sources resolve Paths.get( p )

          if (Files.exists( folder ) && Files.isDirectory( folder ) && Files.isReadable( folder ))
            processDirectory( folder )
          else {
            val md = sources resolve Paths.get( s"$p.md" )

            if (!(Files.exists(md) && Files.isRegularFile(md) && Files.isReadable(md)))
              problem( s"markdown file not found or is not readable: $md" )

            processFile( md )
          }
        }
      case _ => problem( "expected list of paths for 'pages' configuration" )
    }

    scanDirectory( sources )

    for (MdFile( dir, filename, _, _, headings, _ ) <- mdFiles) {
      def links( headings: List[Heading] ): Buffer[Link] =
        headings map {
          case Heading( heading, id, level, subheadings) =>
            val path =
              if (dir.toString == "")
                s"$filename.html"
              else
                s"$dir/$filename.html"

            Link( path, level, heading, id, links(subheadings) )
        } toBuffer

      val pagelinks = links( headings )

      navLinks ++= pagelinks
      pagetocMap((dir, filename)) = toc( pagelinks )

      for (l <- pagelinks)
        headingtocMap(l.heading) = toc( Seq(l) )
    }
  }

  def toc( links: Seq[Link] ): List[Map[String, Any]] =
    links map {
      case Link( path, _, heading, id, sublinks ) =>
        Map( "path" -> path, "heading" -> heading, "id" -> id, "sublinks" -> toc(sublinks))
    } toList

  def writeSite: Unit = {

    val sitetoc = toc( navLinks )
    val headingtoc = headingtocMap toMap

    create( dstnorm )

    for (MdFile( dir, filename, vars, markdown, _, layout ) <- mdFiles) {
      val dstdir = dstnorm resolve dir
      val pagetoc = pagetocMap((dir, filename))
      val base = 1 to dstdir.getNameCount - dstnorm.getNameCount map (_ => "..") mkString "/"
      val page = backslashRenderer.capture( layout,
        Map(
          "contents" -> markdown,
          "page" -> vars,
          "toc" -> pagetoc,
          "headingtoc" -> headingtoc,
          "sitetoc" -> sitetoc,
          "base" -> base
        ) ++ configs )

      create( dstdir )

      val pagepath = dstdir resolve s"$filename.html"

      info( s"writting page: $pagepath" )
      Files.write( pagepath, page.getBytes(StandardCharsets.UTF_8) )
    }

    for (p <- resFiles) {
      val dstpath = dstnorm resolve (sources relativize p)
      val filename = p.getFileName
      val dstdir = dstpath.getParent

      if (Files.exists(dstpath) && Files.isRegularFile(dstpath) && Files.isReadable(dstpath) &&
        Files.getLastModifiedTime(p).compareTo( Files.getLastModifiedTime(dstpath) ) <= 0)
        info( s"asset '$filename' is up-to-date" )
      else {
        info( s"copying asset '$filename' from ${p.getParent} to $dstdir" )
        Files createDirectories dstdir
        Files.copy( p, dstpath, StandardCopyOption.REPLACE_EXISTING )
      }
    }

  }

  def build: Unit = {
    readSources
    writeSite
  }

  def clean: Unit = {
    def clean( dir: Path ): Unit = {
      for (f <- (Files list dir).iterator.asScala) {
        if (Files isDirectory f)
          clean( f )
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

  def info( msg: String ): Unit =
    if (verbose)
      println( msg )

  def headings( doc: Node ) = {
    case class HeadingMutable( heading: String, id: String, level: Int, subheadings: ListBuffer[HeadingMutable] )

    val buf = HeadingMutable( "", "", 0, new ListBuffer[HeadingMutable] )
    var trail: List[HeadingMutable] = List( buf )

    def addHeading( n: Node ): Unit = {
      val level = n.label.substring( 1 ).toInt

      if (tocmin <= level && level <= tocmax)
        if (level > trail.head.level) {
          val sub = HeadingMutable( n.child.mkString, n.attribute("id").get.mkString, level,
            new ListBuffer[HeadingMutable] )

          trail.head.subheadings += sub
          trail = sub :: trail
        } else if (level == trail.head.level) {
          val sub = HeadingMutable( n.child.mkString, n.attribute("id").get.mkString, level,
            new ListBuffer[HeadingMutable] )

          trail.tail.head.subheadings += sub
          trail = sub :: trail.tail
        } else {
          val sub = HeadingMutable( n.child.mkString, n.attribute("id").get.mkString, level,
            new ListBuffer[HeadingMutable] )

          do {
            trail = trail.tail
          } while (trail.head.level >= level)

          addHeading( n )
        }
    }

    def headings( doc: Node ): Unit =
      doc match {
        case e@Elem( _, label, attribs, _, child @ _* ) =>
          label match {
            case "h1"|"h2"|"h3"|"h4"|"h5"|"h6" => addHeading( e )
            case _ => child foreach headings
          }
        case Group( s ) => s foreach headings
        case _ =>
      }

    def list( b: ListBuffer[HeadingMutable] ): List[Heading] =
      if (b isEmpty)
        Nil
      else
        b map {case HeadingMutable( heading, id, level, subheadings ) =>
          Heading( heading, id, level, list(subheadings) )} toList

    headings( doc )
    list( buf.subheadings )
  }

  def processFile( f: Path ): Unit = {
    info( s"reading markdown: $f" )

    val filename = withoutExtension( f.getFileName.toString )
    val s = io.Source.fromFile( f.toFile ).mkString
    val (front, src) = {
      val lines = s.lines

      if (lines.hasNext && lines.next == "---") {
        (yaml.read( lines takeWhile( _ != "---" ) mkString "\n" ).head match {
          case m: Map[_, _] => m.asInstanceOf[Map[String, Any]]
          case _ => problem( s"expected an object as front matter: $f" )
        }, lines mkString "\n")
      } else
        (Map[String, Any](), s)
    }

    val (md, hs) = {
      val xml = Markdown.asXML( src )

      (xml.toString, headings( xml ))
    }

    val layout =
      front get "layout" match {
        case None =>
          if (filename == "index")
            layouts get "index" match {
              case None =>
                layouts get "page" match {
                  case None => problem( s"neither 'index' nor 'page' layout found for laying out $f" )
                  case Some( l ) => l
                }
              case Some( l ) => l
            }
          else
            layouts get "page" match {
              case None => problem( s"'page' layout not found for laying out $f" )
              case Some( l ) => l
            }
        case Some( l ) =>
          layouts get l.toString match {
            case None => problem( s"layout not found: $l in file $f" )
            case Some( ast ) => ast
          }
      }

    val vars = front -- Builder.predefinedFrontmatterKeys

    mdFiles += MdFile( sources relativize f.getParent, filename, vars, md, hs, layout )
  }

  def scanDirectory( dir: Path ): Unit = {
    info( s"scanning directory for assets: $dir" )

    val contents = listDirectory( dir )
    val subdirectories = contents filter (d => Files.isDirectory(d) && Files.isReadable(d))
    val files = contents filter (f => Files.isRegularFile(f) && Files.isReadable(f))

    resFiles ++= files filter (f => !f.getFileName.toString.endsWith(".md"))
    subdirectories foreach scanDirectory
  }

  def listDirectory( dir: Path ) =
    (Files list dir).iterator.asScala.toList filterNot (_.getFileName.toString startsWith "_") sorted

  def processDirectory( dir: Path ): Unit = {
    info( s"searching directory for markdown: $dir" )

    val contents = listDirectory( dir )
    val subdirectories = contents filter (d => Files.isDirectory(d) && Files.isReadable(d))
    val files = contents filter (f => Files.isRegularFile(f) && Files.isReadable(f))
    val mds = files filter (f => f.getFileName.toString.endsWith(".md"))

    if (mds nonEmpty)
      mds foreach processFile

    subdirectories foreach processDirectory
  }

}