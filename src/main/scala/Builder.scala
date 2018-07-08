package xyz.hyperreal.docs

import java.nio.file.Path


class Builder( src: Path, dst: Path, dryrun: Boolean = false, verbose: Boolean = false ) {

  val srcnorm = src.normalize
  val srcdir = srcnorm.toFile

  require( srcdir.isDirectory, s"source path is not a directory: $srcdir" )
  require( srcdir.canRead, s"source directory is unreadable: $srcdir" )

  val dstnorm = dst.normalize
  val dstdir = dstnorm.toFile

  dstdir.mkdirs
  require( dstdir.isDirectory, s"destination path is not a directory: $dstdir" )
  require( dstdir.canWrite, s"destination directory is unwritable: $dstdir" )

  processDirectory( srcnorm, "" )

  def verbosely( msg: String ): Unit =
    if (dryrun || verbose)
      println( msg )

  def processDirectory( parent: Path, sub: String ): Unit = {
    val dir = parent resolve sub

    verbosely( s"processing directory: $dir" )

    val dirfile = dir.toFile
    val contents = dirfile.listFiles.toList filterNot (_.getName startsWith "_")
    val subdirectories = contents filter (d => d.isDirectory && d.canRead)
    val files = contents filter (f => f.getName.endsWith(".md") && f.isFile && f.canRead)

    if (files isEmpty)
      verbosely( "no markdown files" )
    else
      for (f <- files) {
        verbosely( s"processing markdown file: $f" )
      }

    for (s <- subdirectories)
      processDirectory( dir, s.getName )
  }

}