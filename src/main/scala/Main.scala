package xyz.hyperreal.docs

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import xyz.hyperreal.args.Options


object Main extends App {

  if (args isEmpty)
    usage( 1 )

  var verbose = false
  var src: String = null
  var dst: String = null

  try {
    Options( args ) {
      case "init" :: src :: theme :: Nil =>
        val dir = Paths get src

        init( dir, theme )
        println( s"Initialized Docs project in ${dir.toAbsolutePath}${File.separator} with '$theme' theme." )
        sys.exit
      case "-v" :: t =>
        verbose = true
        t
      case "-o" :: d :: t =>
        dst = d
        t
      case ("-h"|"-help"|"--help") :: _ => usage( 0 )
      case s :: _ if s startsWith "-" => sys.error( s"invalid switch $s" )
      case file :: t =>
        src = file
        t
    }

    if (src eq null)
      usage( 1 )

    val srcpath = Paths get src
    val dstpath =
      if (dst eq null)
        srcpath resolve "dst"
      else
        Paths get dst
    val site = new Builder( srcpath, dstpath, verbose )

    site.build
  } catch {
    case e: DocsException => exit( e.getMessage )
  }

  def usage( st: Int ) = {
    println(
      """
        |Docs v0.1
        |Usage: java -jar docs-0.1.jar <document folder> [<destination folder>]
      """.trim.stripMargin )
    sys.exit( st )
  }

  def init( dir: Path, theme: String ): Unit = {
    check( !(Files exists dir), s"file or directory already exists: $dir" )

    val resources =
      for (file <- io.Source.fromInputStream(getClass.getResourceAsStream(s"themes/$theme/contents")).getLines)
        yield {
          val resource = getClass.getResourceAsStream( s"themes/$theme/$file" )

          check( resource != null, s"resource does not exist: themes/$theme/$file" )
          (resource, file)
        }

    create( dir resolve "config" )
    create( dir resolve "layouts" )
    create( dir resolve "includes" )
    create( dir resolve "src" resolve "css" )
    create( dir resolve "src" resolve "js" )
    create( dir resolve "src" resolve "images" )

    for ((resource, file) <- resources) {
      val filepath = file split "/" mkString File.separator

      Files.copy( resource, dir resolve filepath )
    }

    val index =
      s"""---
         |title: Documentation
         |---
         |
         |Overview
         |========
         |
         |""".stripMargin

    Files.write( dir resolve "src" resolve "index.md", index.getBytes(StandardCharsets.UTF_8) )
  }
}