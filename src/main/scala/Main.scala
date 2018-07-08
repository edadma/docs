package xyz.hyperreal.docs

import java.nio.file.Paths


object Main extends App {

  val site = new Builder( Paths get "docs-src", Paths get "docs", dryrun = false, verbose = true )

  site.clean
  site.build

}