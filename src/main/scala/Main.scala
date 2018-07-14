package xyz.hyperreal.docs

import java.nio.file.Paths

import xyz.hyperreal.args.Options


object Main extends App {

  if (args isEmpty) {
    println(
      """
        |Docs v0.1
        |
        |Usage: java -jar docs-0.1.jar <document folder> [<destination folder>]
      """.trim.stripMargin )
    sys.exit( 1 )
  }

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
    case s :: _ if s startsWith "-" => sys.error( s"invalid switch $s" )
    case file :: t =>
      src = file
      t
  }

  val site = new Builder( Paths get src, Paths get dst, verbose )

  site.build

  def init: Unit = {

  }
}