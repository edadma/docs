//@
package xyz.hyperreal.docs

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.jdk.CollectionConverters._
import xyz.hyperreal.{backslash, markdown, yaml}
import xyz.hyperreal.markdown.{HeadingAST, Markdown, SeqAST, Util}
import xyz.hyperreal.backslash.{Command, Parser, Renderer}


class Builder( src: Path, dst: Path, verbose: Boolean = false, clean: Boolean = false ) {

  val frontmatterKeys = Set( "template", "rendertoc", "toc" )
  val defaultPermalink = s":directory/:filename"
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
  val permalinkParser = new Parser( Command.standard, csDelim = ":" )
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
            List( (withoutExtension(l.getFileName), yml) )
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

          (withoutExtension(l.getFileName), backslashParser.parse( io.Source.fromFile(l.toFile) ))
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

  case class SrcFile( dir: Path, filename: String, pagepath: String, vars: Map[String, Any], content: String, headings: Option[Seq[Heading]], template: backslash.AST )

  val srcFiles = new ArrayBuffer[SrcFile]
  val resFiles = new ArrayBuffer[Path]
  val pagetocMap = new mutable.HashMap[String, List[Map[String, Any]]]
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
      case Some( v ) => v
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
      case Some( v ) => v
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
      case Some( v ) => v
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
                problem( s"source file not found or is unreadable: $src" )

              processFile( src )
            }
          case h: Map[_, _] => readStructure( h )
        }
      case hds: Map[_, _] =>
        hds foreach {
          case (k: String, v) => addHeading( 0, None, k, "", sitetrail )
        }
    }

  def readSources: Unit = {
    configs get "document" match {
      case None => processDirectory( sources )
      case Some( s ) => readStructure( s )
    }

    processSpecials( sources )
    scanDirectory( sources )

    srcFiles foreach {
      case SrcFile( _, _, pagepath, _, _, Some(headings), _ ) =>
        pagetocMap(pagepath) = toc( headings )

        for (l <- headings)
          headingtocMap(l.heading) = toc( Seq(l) )
      case _ =>
    }
  }

  def toc( headings: Seq[Heading] ): List[Map[String, Any]] =
    headings map {
      case Heading( Some(path), heading, id, level, sublinks ) =>
        Map( "path" -> path, "heading" -> heading, "id" -> id, "level" -> level, "sublinks" -> toc(sublinks.toSeq) )
      case Heading( None, heading, id, level, sublinks ) =>
        Map( "heading" -> heading, "id" -> id, "level" -> level, "sublinks" -> toc(sublinks.toSeq) )
    } toList

  def writeSite: Unit = {
    val sitetoc = toc( sitebuf.subheadings.toSeq )
    val headingtoc = headingtocMap toMap

    create( dstnorm )

    srcFiles foreach {
      case SrcFile( dir, filename, pagepath, vars, markdown, headings, template ) =>
        val dstdir = dstnorm resolve dir
        val pagetoc =
          if (headings isDefined)
            pagetocMap(pagepath)
          else
            Map()
        val base = 1 to dstdir.getNameCount - dstnorm.getNameCount map (_ => "..") mkString "/"
        val page = backslashRenderer.capture( template,
          Map(
            "content" -> markdown,
            "rendertoc" -> getConfigBoolean( vars, "rendertoc", true ),
            "page" -> (vars -- frontmatterKeys),
            "toc" -> pagetoc,
            "headingtoc" -> headingtoc,
            "sitetoc" -> sitetoc,
            "base" -> base,
            "pagepath" -> pagepath
          ) ++ configs )

        create( dstdir )

        val pagedstpath = dstdir resolve filename

        info( s"writing page: $pagedstpath" )
        Files.write( pagedstpath, page.getBytes(StandardCharsets.UTF_8) )
    }

    for (p <- resFiles) {
      val dstpath = dstnorm resolve (sources relativize p)
      val filename = p.getFileName
      val dstdir = dstpath.getParent

      if (filename.toString endsWith ".backslash") {
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

  def withoutExtension( f: Path ) = {
    val s = f.toString

    s lastIndexOf '.' match {
      case -1 => s
      case dot => s.substring( 0, dot )
    }
  }

  def info( msg: String ): Unit =
    if (verbose)
      println( msg )

  case class Heading( path: Option[String], heading: String, id: String, level: Int, subheadings: ListBuffer[Heading] )

  val sitebuf = Heading( None, "", "", -1, new ListBuffer[Heading] )
  val sitetrail: mutable.Stack[Heading] = mutable.Stack( sitebuf )
  val firstHeading = new mutable.HashSet[String]

  def headings( path: Option[String], doc: markdown.AST, front: Map[String, Any] ) = {
    val pagebuf = Heading( path, "", "", -1, new ListBuffer[Heading] )
    val pagetrail: mutable.Stack[Heading] = mutable.Stack( pagebuf )

    def addNodeHeading( level: Int, text: String, id: String, trail: mutable.Stack[Heading], tocmin: Int, tocmax: Int ): Unit =
      if (tocmin <= level && level <= tocmax)
        addHeading( level, path, text, id, trail )

    def headings( doc: markdown.AST ): Unit =
      doc match {
        case HeadingAST( level, contents, id ) =>
          val first = getConfigBoolean( front, "toc.first-heading-fragment", true )
          val id1 =
            if (path isDefined)
              if (first || firstHeading(path.get))
                id.get
              else {
                firstHeading += path.get
                ""
              }
            else
              ""
          val (ptmin, ptmax) =
            (getConfigInt( front, "pagetoc.min-level", 0 ),
              getConfigInt( front, "pagetoc.max-level", 6 ))
          val (tmin, tmax) =
            (getConfigInt( front, "toc.min-level", 0 ),
              getConfigInt( front, "toc.max-level", 6 ))

          addNodeHeading( level, Util.text(contents), id.get, pagetrail, ptmin, ptmax )
          addNodeHeading( level, Util.text(contents), id1, sitetrail, tmin, tmax )
        case SeqAST( s ) => s foreach headings
        case _ =>
      }

    headings( doc )
    pagebuf.subheadings
  }

  def addHeading( level: Int, path: Option[String], text: String, id: String, trail: mutable.Stack[Heading] ): Unit = {
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

  def transformMarkdown( src: String, pagepath: String, front: Map[String, Any] ) = {
    val doc = Markdown( src )

    (Util.html( doc, 2, codeblock ), Some( headings(Some(pagepath), doc, front).toSeq ))
  }

  def transformHTML( src: String, pagepath: String, front: Map[String, Any] ) = (src, None)

//  def path( dir: Path, filename: String ) =
//    (dir.toString, filename) match {
//      case ("", "index") => "."
//      case ("", _) => filename
//      case (d, "index") => d
//      case _ => s"$dir/$filename"
//    }

  def processSourceFile( f: Path, transform: (String, String, Map[String, Any]) => (String, Option[Seq[Heading]]) ): Unit = {
    info( s"reading source: $f" )

    val filename = withoutExtension( f.getFileName )
    val text = io.Source.fromFile( f.toFile )
    val lines = text.getLines
    val (front, src) = {

      if (lines.hasNext && lines.next == "---") {
        (yaml.read( lines takeWhile( _ != "---" ) mkString "\n" ).head match {
          case m: Map[_, _] => m.asInstanceOf[Map[String, Any]]
          case _ => problem( s"expected an object as front matter: $f" )
        }, lines mkString "\n")
      } else
        (Map[String, Any](), text.mkString)
    }

    val dir = sources relativize f.getParent
    val pagepath = {
      val s = backslashRenderer.capture(
        permalinkParser.parse(io.Source.fromString(getConfigString(front, "permalink", defaultPermalink))),
        Map("directory" -> dir.toString, "filename" -> filename) ).trim
      val s1 = if (s startsWith "/") s drop 1 else s

      if (s1 endsWith "index") s1.substring( 0, s1.length - "index".length ) else s1
    }
    val permalink = {
      val s = if (pagepath.isEmpty || (pagepath endsWith "/")) pagepath + "index" else pagepath

      Paths.get( s.replace('/', File.separatorChar) + ".html" )
    }
    val permalinkdir =
      if (permalink.getNameCount == 1)
        Paths get ""
      else
        permalink.getParent
    val permalinkfilename = permalink.getFileName.toString
    val (res, hs) = transform( src, pagepath, front )
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

    srcFiles += SrcFile( permalinkdir, permalinkfilename, pagepath, front, res, hs, template )
  }

  def processFile( f: Path ): Unit =
    if (isMarkdown( f ))
      processSourceFile( f, transformMarkdown )
    else if (isHTML( f ))
      processSourceFile( f, transformHTML )
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

  val specialSources = List( "403", "404" )

  def processSpecials( dir: Path ): Unit = {
    info( s"scanning directory for special sources: $dir" )

    for (f <- listDirectory( dir ))
      if (specialSources.contains( withoutExtension(f.getFileName) ) && Files.isRegularFile( f ) && Files.isReadable( f ))
        processFile( f )
  }

  def processDirectory( dir: Path ): Unit = {
    info( s"scanning directory for sources: $dir" )

    for (f <- listDirectory( dir ))
      if (Files.isDirectory( f ) && Files.isReadable( f ))
        processDirectory( f )
      else if (Files.isRegularFile( f ) && Files.isReadable( f ) && (isMarkdown( f ) || isHTML( f )))
        processFile( f )
      else
        info( s"ignoring $f" )
  }

}
