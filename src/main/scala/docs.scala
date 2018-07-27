package xyz.hyperreal

import java.nio.file.{Files, Path}


package object docs {

  class DocsException( msg: String ) extends Exception( msg )

  def exit( msg: String ): Unit = {
    println( msg )
    sys.exit( 1 )
  }

  def check( cond: Boolean, msg: String ) =
    if (!cond)
      problem( msg )

  def problem( msg: String ) = sys.error(msg)//throw new DocsException( msg )

  def create( dir: Path ) = {
    Files createDirectories dir
    check( Files.exists(dir) && Files.isDirectory(dir), s"failed to create directory: $dir" )
    check( Files isWritable dir, s"directory is unwritable: $dir" )
  }
}