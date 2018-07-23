//@
package xyz.hyperreal.docs

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ArrayStack, ListBuffer}
import scala.collection.JavaConverters._
import xyz.hyperreal.{backslash, markdown, yaml}
import xyz.hyperreal.markdown.{HeadingAST, Markdown, SeqAST, Util}
import xyz.hyperreal.backslash.{Command, Parser, Renderer}


object Builder {

  val predefinedFrontmatterKeys = Set( "template" )

}

class Builder( src: Path, dst: Path, verbose: Boolean = false, clean: Boolean = false ) {

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

  val templatedir = srcnorm resolve "templates"

  check( Files exists templatedir, s"'templates directory does not exist: $templatedir" )
  check( Files isDirectory templatedir, s"not a directory: $templatedir" )
  check( Files isReadable templatedir, s"templates directory is unreadable: $templatedir" )

  val configdir = srcnorm resolve "config"

  check( Files exists configdir, s"'config directory does not exist: $configdir" )
  check( Files isDirectory configdir, s"not a directory: $configdir" )
  check( Files isReadable configdir, s"config directory is unreadable: $configdir" )

  val sources = srcnorm resolve "src"

  check( Files exists sources, s"'sources directory does not exist: $sources" )
  check( Files isDirectory sources, s"not a directory: $sources" )
  check( Files isReadable sources, s"sources directory is unreadable: $sources" )

  info( s"processing configs: $configdir" )

  val configs = {
    val yamls = (Files list configdir).iterator.asScala.toList.filter (p => Files.isRegularFile(p) &&
      Files.isReadable(p) && (p.getFileName.toString.endsWith(".yml") || p.getFileName.toString.endsWith(".yaml")))
    val cs =
      for (l <- yamls)
        yield {
          info( s"reading config: $l" )

          val yml = yaml.read( io.Source.fromFile(l toFile) ).head

          if (yml == null)
            Nil
          else
            List( (withoutExtension(l.getFileName.toString), yml) )
        }

    cs.flatten toMap
  }
  val settings = configs get "settings" match {
    case Some( s: Map[_, _] ) => s
    case _ => problem( s"'settings' object not found in configs" )
  }

  info( s"processing templates: $templatedir" )

  val templates = {
    val templates = (Files list templatedir).iterator.asScala.toList.filter (p => Files.isRegularFile(p) && Files.isReadable(p) && p.getFileName.toString.endsWith(".backslash"))
    val ls =
      for (l <- templates)
        yield {
          info( s"reading template: $l" )

          (withoutExtension(l.getFileName.toString), backslashParser.parse( io.Source.fromFile(l.toFile) ))
        }

    ls toMap
  }

  val normalCodeBlock = templates get "normal-code-block" match {
    case Some( l ) => l
    case _ => problem( s"'normal-code-block' template not found in templates" )
  }
  val captionedCodeBlock = templates get "captioned-code-block" match {
    case Some( l ) => l
    case _ => problem( s"'captioned-code-block' template not found in templates" )
  }

  trait SrcFile

  case class MdFile( dir: Path, filename: String, vars: Map[String, Any], content: String, headings: Seq[Heading], template: backslash.AST ) extends SrcFile
  case class HtmlFile( dir: Path, filename: String, vars: Map[String, Any], content: String, template: backslash.AST ) extends SrcFile

  val srcFiles = new ArrayBuffer[SrcFile]
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

  def getBooleanOption( store: Any, name: String ) =
    getValue( store, name ) match {
      case None => None
      case Some( v: Boolean ) => Some( v )
      case Some( v ) => problem( s"expected boolean, got $v for $name" )
    }

  def getBoolean( store: Any, name: String ) =
    getBooleanOption( store, name ) match {
      case None => problem( s"required field $name not found" )
      case Some( v ) => v
    }

  def getBooleanDefault( store: Any, name: String, default: Boolean ) =
    getBooleanOption( store, name ) match {
      case None => default
      case Some( v ) => v
    }

  def getConfigBoolean( front: Map[String, Any], name: String, default: Boolean ) =
    getBooleanOption( front, name ) match {
      case None => getBooleanDefault( settings, name, default )
    }

  def getStringOption( store: Any, name: String ) =
    getValue( store, name ) match {
      case None => None
      case Some( v: String ) => Some( v )
      case Some( v ) => problem( s"expected String, got $v for $name" )
    }

  def getString( store: Any, name: String ) =
    getStringOption( store, name ) match {
      case None => problem( s"required field $name not found" )
      case Some( v ) => v
    }

  def getStringDefault( store: Any, name: String, default: String ) =
    getStringOption( store, name ) match {
      case None => default
      case Some( v ) => v
    }

  def getConfigString( front: Map[String, Any], name: String, default: String ) =
    getStringOption( front, name ) match {
      case None => getStringDefault( settings, name, default )
    }

  def readStructure( struct: Any ): Unit =
    struct match {
      case pgs: List[_] =>
        pgs foreach {
          case p: String =>
            val src = sources resolve Paths.get( p )

            if (Files.exists( src ) && Files.isDirectory( src ) && Files.isReadable( src ))
              processDirectory( src )
            else {
              if (!(Files.exists(src) && Files.isRegularFile(src) && Files.isReadable(src)))
                problem( s"markdown file not found or is not readable: $src" )

              processFile( src )
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

    srcFiles foreach {
      case MdFile( dir, filename, _, _, headings, _ ) =>
        pagetocMap((dir, filename)) = toc( headings )

        for (l <- headings)
          headingtocMap(l.heading) = toc( Seq(l) )
      case _ =>
    }
  }

  def toc( headings: Seq[Heading] ): List[Map[String, Any]] =
    headings map {
      case Heading( path, heading, id, level, sublinks ) =>
        Map( "path" -> path, "heading" -> heading, "id" -> id, "level" -> level, "sublinks" -> toc(sublinks))
    } toList

  def path( dir: Path, filename: String ) =
    (dir.toString, filename) match {
      case ("", "index") => "."
      case ("", _) => s"$filename.html"
      case (d, "index") => d
      case _ => s"$dir/$filename.html"
    }

  def writeSite: Unit = {

    val sitetoc = toc( sitebuf.subheadings )
    val headingtoc = headingtocMap toMap

    create( dstnorm )

    srcFiles foreach {
      case MdFile( dir, filename, vars, markdown, _, template ) =>
        val dstdir = dstnorm resolve dir
        val pagetoc = pagetocMap((dir, filename))
        val base = 1 to dstdir.getNameCount - dstnorm.getNameCount map (_ => "..") mkString "/"
        val page = backslashRenderer.capture( template,
          Map(
            "content" -> markdown,
            "page" -> vars,
            "toc" -> pagetoc,
            "headingtoc" -> headingtoc,
            "sitetoc" -> sitetoc,
            "base" -> base,
            "pagepath" -> path( dir, filename )
          ) ++ configs )

        create( dstdir )

        val pagepath = dstdir resolve s"$filename.html"

        info( s"writting page: $pagepath" )
        Files.write( pagepath, page.getBytes(StandardCharsets.UTF_8) )
      case HtmlFile( dir, filename, vars, html, template ) =>
        val dstdir = dstnorm resolve dir
        val base = 1 to dstdir.getNameCount - dstnorm.getNameCount map (_ => "..") mkString "/"
        val page = backslashRenderer.capture( template,
          Map(
            "content" -> html,
            "page" -> vars,
            "toc" -> Map(),
            "headingtoc" -> headingtoc,
            "sitetoc" -> sitetoc,
            "base" -> base,
            "pagepath" -> path( dir, filename )
          ) ++ configs )

        create( dstdir )

        val pagepath = dstdir resolve s"$filename.html"

        info( s"writting page: $pagepath" )
        Files.write( pagepath, page.getBytes(StandardCharsets.UTF_8) )
    }

    for (p <- resFiles) {
      val dstpath = dstnorm resolve (sources relativize p)
      val filename = p.getFileName.toString
      val dstdir = dstpath.getParent

      if (filename endsWith ".backslash") {
        val respath = dstdir resolve withoutExtension( filename )

        info( s"transforming asset '$filename' in ${p.getParent} and writing $respath" )

        val asset = backslashRenderer.capture( backslashParser.parse( io.Source.fromFile(p.toFile) ), configs )

        Files.write( respath, asset.getBytes(StandardCharsets.UTF_8) )
      } else if (Files.exists(dstpath) && Files.isRegularFile(dstpath) && Files.isReadable(dstpath) &&
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
  val firstHeading = new mutable.HashSet[String]

  def headings( path: String, doc: markdown.AST, front: Map[String, Any] ) = {
    val pagebuf = Heading( path, "", "", -1, new ListBuffer[Heading] )
    val pagetrail: ArrayStack[Heading] = ArrayStack( pagebuf )

    def addNodeHeading( level: Int, text: String, id: String, trail: ArrayStack[Heading], tocmin: Int, tocmax: Int ): Unit =
      if (tocmin <= level && level <= tocmax)
        addHeading( level, path, text, id, trail )

    def headings( doc: markdown.AST ): Unit =
      doc match {
        case HeadingAST( level, contents, id ) =>
          val first = getConfigBoolean( front, "toc.first-heading-fragment", true )
          val id1 =
            if (first || firstHeading(path))
              id.get
            else {
              firstHeading += path
              ""
            }
          val (ptmin, ptmax) =
            getStringOption( front, "pagetoc" ) match {
              case Some( "disabled" ) => (Int.MaxValue, 0)
              case _ =>
                (getConfigInt( front, "pagetoc.min-level", 0 ),
                  getConfigInt( front, "pagetoc.max-level", 6 ))
            }
          val (tmin, tmax) =
            getStringOption( front, "toc" ) match {
              case Some( "disabled" ) => (Int.MaxValue, 0)
              case _ =>
                (getConfigInt( front, "toc.min-level", 0 ),
                  getConfigInt( front, "toc.max-level", 6 ))
            }

          addNodeHeading( level, Util.text(contents), id.get, pagetrail, ptmin, ptmax )
          addNodeHeading( level, Util.text(contents), id1, sitetrail, tmin, tmax )
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

  def codeblock( c: String, highlighted: Option[String], captioned: Option[String]) =
    backslashRenderer.capture(
      if (captioned.isDefined) captionedCodeBlock else normalCodeBlock,
      Map( "content" -> c, "caption" -> captioned.orNull ) )

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

      (Util.html( doc, 2, codeblock ), headings( path(dir, filename), doc, front ))
    }

    val template =
      front get "template" match {
        case None =>
          if (filename == "index")
            templates get "index" match {
              case None =>
                templates get "page" match {
                  case None => problem( s"neither 'index' nor 'page' template found for laying out $f" )
                  case Some( l ) => l
                }
              case Some( l ) => l
            }
          else
            templates get "page" match {
              case None => problem( s"'page' template not found for laying out $f" )
              case Some( l ) => l
            }
        case Some( l ) =>
          templates get l.toString match {
            case None => problem( s"template not found: $l in file $f" )
            case Some( ast ) => ast
          }
      }

    val vars = front -- Builder.predefinedFrontmatterKeys

    srcFiles += MdFile( dir, filename, vars, md, hs, template )
  }

  def processHTMLFile( f: Path ): Unit = {
    info( s"reading HTML: $f" )

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

    val template =
      front get "template" match {
        case None =>
          if (filename == "index")
            templates get "index" match {
              case None =>
                templates get "page" match {
                  case None => problem( s"neither 'index' nor 'page' template found for laying out $f" )
                  case Some( l ) => l
                }
              case Some( l ) => l
            }
          else
            templates get "page" match {
              case None => problem( s"'page' template not found for laying out $f" )
              case Some( l ) => l
            }
        case Some( l ) =>
          templates get l.toString match {
            case None => problem( s"template not found: $l in file $f" )
            case Some( ast ) => ast
          }
      }

    val vars = front -- Builder.predefinedFrontmatterKeys

    srcFiles += HtmlFile( dir, filename, vars, src, template )
  }

  def processFile( f: Path ): Unit =
    if (isMarkdown( f ))
      processMarkdownFile( f )
    else if (isHTML( f ))
      processHTMLFile( f )
    else
      problem( s"don't know how to process $f" )

  def hasExtension( f: Path, ext: String* ) = ext exists (f.getFileName.toString.endsWith( _ ))

  def isMarkdown( f: Path ) = hasExtension( f, ".md", ".markdown" )

  def isHTML( f: Path ) = hasExtension( f, ".html", ".htm" )

  def scanDirectory( dir: Path ): Unit = {
    info( s"scanning directory for assets: $dir" )

    val contents = listDirectory( dir )
    val subdirectories = contents filter (d => Files.isDirectory(d) && Files.isReadable(d))
    val files = contents filter (f => Files.isRegularFile(f) && Files.isReadable(f))

    resFiles ++= files filter (f => !(isMarkdown(f) || f.getFileName.toString.endsWith(".html")))
    subdirectories foreach scanDirectory
  }

  def listDirectory( dir: Path ) =
    (Files list dir).iterator.asScala.toList sorted

  def processDirectory( dir: Path ): Unit = {
    info( s"scanning directory for sources: $dir" )

    for (f <- listDirectory( dir ))
      if (Files.isDirectory( f ) && Files.isReadable( f ))
        processDirectory( f )
      else if (Files.isRegularFile( f ) && Files.isReadable( f ))
        processFile( f )
      else
        info( s"ignoring $f" )
  }

}