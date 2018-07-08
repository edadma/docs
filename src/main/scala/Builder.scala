//@
package xyz.hyperreal.docs

import java.nio.file.Path

import xyz.hyperreal.markdown.Markdown
import xyz.hyperreal.backslash.Parser


class Builder( src: Path, dst: Path, dryrun: Boolean = false, verbose: Boolean = false ) {

  val srcnorm = src.normalize
  val srcdir = srcnorm.toFile

  require( srcdir.exists, s"source does not exist: $srcdir" )
  require( srcdir.isDirectory, s"source path is not a directory: $srcdir" )
  require( srcdir.canRead, s"source directory is unreadable: $srcdir" )

  val dstnorm = dst.normalize
  val dstdir = dstnorm.toFile

  dstdir.mkdirs
  require( dstdir.isDirectory, s"destination path is not a directory: $dstdir" )
  require( dstdir.canWrite, s"destination directory is unwritable: $dstdir" )

  val layoutdir = srcnorm resolve "_layout" toFile

  require( layoutdir.exists, s"'layout directory does not exist: $layoutdir" )
  require( layoutdir.isDirectory, s"not a directory: $layoutdir" )
  require( layoutdir.canRead, s"layout directory is unreadable: $layoutdir" )

  verbosely( s"processing layouts: $layoutdir" )

  val layouts = {
    for (l <- layoutdir.listFiles filter (f => f.isFile && f.canRead && f.getName.endsWith(".backslash"))) {
      verbosely( s"processing layout: $l" )

//      val layout = Parser
    }
  }

  //      .map( f => (withoutExtension(f.getName), {)io.Source.fromFile(f)) )

  processDirectory( srcnorm, "" )

  def withoutExtension( s: String ) =
    s lastIndexOf '.' match {
      case -1 => s
      case dot => s.substring( 0, dot )
    }

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

        val s = io.Source.fromFile( f ).mkString

        val (md, headings) = Markdown.withHeadings( s )

        if (!dryrun) {

        }
      }

    for (s <- subdirectories)
      processDirectory( dir, s.getName )
  }

}