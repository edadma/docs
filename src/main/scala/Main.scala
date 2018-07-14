package xyz.hyperreal.docs

import java.nio.file.{Files, Path, Paths}
import java.net.URI

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
        init( Paths get src, theme )
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
        srcpath resolve dst
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
    check( !(Files exists dir), s"error initializing - file or directory already exists: $dir" )

//    case class Dir( name: String, sub: List[Dir] = Nil )
//
//    def dirs( parent: Path, child: Dir ): Unit = {
//      val dir = parent resolve child.name
//
//      create( dir )
//
//      for (c <- child.sub)
//        dirs( dir, c )
//    }
//
//    val struct =
//      Dir( "", List(
//        Dir( "config" ),
//        Dir( "layouts" ),
//        Dir( "includes" ),
//        Dir( "src", List(
//          Dir( "css" ),
//          Dir( "images" )
//        ))
//      ))
//
//    dirs( dir, struct )
    create( dir resolve "config" )
    create( dir resolve "layouts" )
    create( dir resolve "includes" )
    create( dir resolve "src" resolve "css" )
    create( dir resolve "src" resolve "images" )

    for (file <- io.Source.fromInputStream(getClass.getResourceAsStream(s"themes/$theme/contents")).getLines) {
      val resource = getClass.getResourceAsStream( s"themes/$theme/$file" )

      println( file )
      check( resource != null, s"resource does not exist: themes/$theme/$file" )
      Files.copy( resource, dir resolve Paths.get(new URI(file)) )
    }
  }
}