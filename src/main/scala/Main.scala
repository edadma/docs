package xyz.hyperreal.docs

import java.nio.file.Paths

import xyz.hyperreal.args.Options


object Main extends App {

  if (args isEmpty)
    usage( 1 )

  var verbose = false
  var src: String = null
  var dst: String = null

  Options( args ) {
    case "init" :: src :: Nil =>
      init
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

  def usage( st: Int ) = {
    println(
      """
        |Docs v0.1
        |Usage: java -jar docs-0.1.jar <document folder> [<destination folder>]
      """.trim.stripMargin )
    sys.exit( st )
  }

  def init: Unit = {

  }
}