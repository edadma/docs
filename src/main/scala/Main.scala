package xyz.hyperreal.docs

import java.nio.file.Paths


object Main extends App {

  val builder = new Builder( Paths.get("docs-src"), Paths.get("docs"), dryrun = true )

}