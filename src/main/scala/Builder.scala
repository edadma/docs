//@
package xyz.hyperreal.docs

import java.nio.file.Path

import xyz.hyperreal.yaml.read
import xyz.hyperreal.markdown.Markdown
import xyz.hyperreal.backslash.{Command, Parser}


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
  val backslashParser = new Parser( Command.standard )

  require( layoutdir.exists, s"'layout directory does not exist: $layoutdir" )
  require( layoutdir.isDirectory, s"not a directory: $layoutdir" )
  require( layoutdir.canRead, s"layout directory is unreadable: $layoutdir" )

  verbosely( s"processing layouts: $layoutdir" )

  val layouts = {
    val ls =
      for (l <- layoutdir.listFiles filter (f => f.isFile && f.canRead && f.getName.endsWith(".backslash")))
        yield {
          verbosely( s"processing layout: $l" )

          (withoutExtension(l.getName), backslashParser.parse( io.Source.fromFile(l) ))
        }

    ls toMap
  }

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
        val (front, src) = {
          val lines = s.lines

          if (lines.hasNext && lines.next == "---") {
            (read( lines takeWhile( _ != "---" ) mkString "\n" ), lines mkString "\n")
          } else
            (Map(), s)
        }

        println( front, src )
        val (md, headings) = Markdown.withHeadings( src )

        if (!dryrun) {

        }
      }

    for (s <- subdirectories)
      processDirectory( dir, s.getName )
  }

}