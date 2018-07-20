//@
package xyz.hyperreal.docs

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ArrayStack, ListBuffer}
import scala.collection.JavaConverters._
import xyz.hyperreal.{backslash, yaml, markdown}
import xyz.hyperreal.markdown.{HeadingAST, Markdown, SeqAST, Util}
import xyz.hyperreal.backslash.{Command, Parser, Renderer}

import scala.xml.{Elem, Group, Node, Text}


object Builder {

  val predefinedFrontmatterKeys = Set( "layout" )

}

class Builder( src: Path, dst: Path, verbose: Boolean = false, clean: Boolean ) {

  val srcnorm = src.normalize.toAbsolutePath

  check( Files exists srcnorm, s"source does not exist: $srcnorm" )
  check( Files isDirectory srcnorm, s"source path is not a directory: $srcnorm" )
  check( Files isReadable srcnorm, s"source directory is unreadable: $srcnorm" )

  val dstnorm = dst.normalize.toAbsolutePath
  val backslashConfig =
    Map(
      "today" -> "MMMM d, y",
      "include" -> (srcnorm resolve "includes").toString,
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
    val yamls = (Files list configdir).iterator.asScala.toList.filter (p => Files.isRegularFile(p) &&
      Files.isReadable(p) && (p.getFileName.toString.endsWith(".yml") || p.getFileName.toString.endsWith(".yaml")))
    val cs =
      for (l <- yamls)
        yield {
          info( s"reading config: $l" )

          val yml = new String( Files readAllBytes l, StandardCharsets.UTF_8 ) trim

          if (yml isEmpty)
            Nil
          else
            List( (withoutExtension(l.getFileName.toString), yaml.read( yml ).head) )
        }

    cs.flatten toMap
  }
  val settings = configs get "settings" match {
    case Some( s: Map[_, _] ) => s
    case _ => problem( s"'settings' object not found in configs" )
  }

  case class MdFile( dir: Path, filename: String, vars: Map[String, Any], md: String, headings: Seq[Heading], layout: backslash.AST )

  val mdFiles = new ArrayBuffer[MdFile]
  val resFiles = new ArrayBuffer[Path]
  val pagetocMap = new mutable.HashMap[(Path, String), List[Map[String, Any]]]
  val headingtocMap = new mutable.HashMap[String, List[Map[String, Any]]]

  def getValue( store: Any, name: String ) = {
    val fields = name split "\\." toList

    def getField( v: Any, fs: List[String] ): Option[Any] =
      (v, fs) match {
        case (_, Nil) => Some( v )
        case (m: Map[_, _], n :: t) =>
          m.asInstanceOf[Map[String, Any]] get n match {
            case None => None
            case Some( m1 ) => getField( m1, t )
          }
        case (_, n :: t) => problem( s"invalid field name: $n in $name" )
      }

    getField( store, fields )
  }

  def getIntOption( store: Any, name: String ) =
    getValue( store, name ) match {
      case None => None
      case Some( v: Int ) => Some( v )
      case Some( v ) => problem( s"expected int, got $v for $name" )
    }

  def getInt( store: Any, name: String ) =
    getIntOption( store, name ) match {
      case None => problem( s"required field $name not found" )
      case Some( v ) => v
    }

  def getIntDefault( store: Any, name: String, default: Int ) =
    getIntOption( store, name ) match {
      case None => default
      case Some( v ) => v
    }

  def getConfigInt( front: Map[String, Any], name: String, default: Int ) =
    getIntOption( front, name ) match {
      case None => getIntDefault( settings, name, default )
    }

  def readStructure( struct: Any ): Unit =
    struct match {
      case pgs: List[_] =>
        pgs foreach {
          case p: String =>
            val folder = sources resolve Paths.get( p )

            if (Files.exists( folder ) && Files.isDirectory( folder ) && Files.isReadable( folder ))
              processDirectory( folder )
            else {
              val md = sources resolve Paths.get( s"$p.md" )

              if (!(Files.exists(md) && Files.isRegularFile(md) && Files.isReadable(md)))
                problem( s"markdown file not found or is not readable: $md" )

              processMarkdownFile( md )
            }
          case h: Map[_, _] => readStructure( h )
        }
      case hds: Map[_, _] =>
        hds foreach {
          case (k: String, v) => addHeading( 0, "", k, "", sitetrail )
        }
    }

  def readSources: Unit = {
    configs get "document" match {
      case None => processDirectory( sources )
      case Some( s ) => readStructure( s )
    }

    scanDirectory( sources )

    for (MdFile( dir, filename, _, _, headings, _ ) <- mdFiles) {
      pagetocMap((dir, filename)) = toc( headings )

      for (l <- headings)
        headingtocMap(l.heading) = toc( Seq(l) )
    }
  }

  def toc( headings: Seq[Heading] ): List[Map[String, Any]] =
    headings map {
      case Heading( path, heading, id, level, sublinks ) =>
        Map( "path" -> path, "heading" -> heading, "id" -> id, "level" -> level, "sublinks" -> toc(sublinks))
    } toList

  def writeSite: Unit = {

    val sitetoc = toc( sitebuf.subheadings )
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
          "base" -> base,
          "pagepath" -> s"$dir/$filename.html"
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

    if (clean)
      cleanSite

    writeSite
  }

  def cleanSite: Unit = {
    def clean( dir: Path ): Unit = {
      for (f <- (Files list dir).iterator.asScala) {
        if (Files isDirectory f)
          clean( f )
        else
          Files delete f
      }

      Files delete dir
    }

    if (Files exists dstnorm) {
      info( s"cleaning target directory: $dstnorm" )
      clean( dstnorm )
    }
  }

  def withoutExtension( s: String ) =
    s lastIndexOf '.' match {
      case -1 => s
      case dot => s.substring( 0, dot )
    }

  def info( msg: String ): Unit =
    if (verbose)
      println( msg )

  case class Heading( path: String, heading: String, id: String, level: Int, subheadings: ListBuffer[Heading] )

  val sitebuf = Heading( "", "", "", -1, new ListBuffer[Heading] )
  val sitetrail: ArrayStack[Heading] = ArrayStack( sitebuf )

  def headings( path: String, doc: markdown.AST, front: Map[String, Any] ) = {
    val pagebuf = Heading( path, "", "", -1, new ListBuffer[Heading] )
    val pagetrail: ArrayStack[Heading] = ArrayStack( pagebuf )

    def addNodeHeading( level: Int, text: String, id: String, trail: ArrayStack[Heading], tocmin: Int, tocmax: Int ): Unit =
      if (tocmin <= level && level <= tocmax)
        addHeading( level, path, text, id, trail )

    def headings( doc: markdown.AST ): Unit =
      doc match {
        case HeadingAST( level, contents, id ) =>
              addNodeHeading( level, Util.text(contents), id.get, pagetrail, getConfigInt(front, "pagetoc.min-level", 0), getConfigInt(front, "pagetoc.max-level", 6) )
              addNodeHeading( level, Util.text(contents), id.get, sitetrail, getConfigInt(front, "toc.min-level", 0), getConfigInt(front, "toc.max-level", 6) )
        case SeqAST( s ) => s foreach headings
        case _ =>
      }

    headings( doc )
    pagebuf.subheadings
  }

  def addHeading( level: Int, path: String, text: String, id: String, trail: ArrayStack[Heading] ): Unit = {
    if (level > trail.head.level) {
      val sub = Heading( path, text, id, level, new ListBuffer[Heading] )

      trail.top.subheadings += sub
      trail push sub
    } else if (level == trail.head.level) {
      val sub = Heading( path, text, id, level, new ListBuffer[Heading] )

      trail.pop
      trail.top.subheadings += sub
      trail push sub
    } else {
      val sub = Heading( path, text, id, level, new ListBuffer[Heading] )

      do {
        trail.pop
      } while (trail.top.level >= level)

      addHeading( level, path, text, id, trail )
    }
  }

  def processMarkdownFile( f: Path ): Unit = {
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

    val dir = sources relativize f.getParent
    val (md, hs) = {
      val doc = Markdown( src )
      val path =
        if (dir.toString == "")
          s"$filename.html"
        else
          s"$dir/$filename.html"

      (Util.html( doc, 2, null ), headings( path, doc, front ))
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

    mdFiles += MdFile( dir, filename, vars, md, hs, layout )
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
    (Files list dir).iterator.asScala.toList sorted

  def processDirectory( dir: Path ): Unit = {
    info( s"scanning directory for sources: $dir" )

    val contents = listDirectory( dir )
    val subdirectories = contents filter (d => Files.isDirectory(d) && Files.isReadable(d))
    val files = contents filter (f => Files.isRegularFile(f) && Files.isReadable(f))
    val mds = files filter (f => f.getFileName.toString.endsWith(".md"))

    if (mds nonEmpty)
      mds foreach processMarkdownFile

    subdirectories foreach processDirectory
  }

}