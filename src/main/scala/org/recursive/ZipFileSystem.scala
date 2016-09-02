package org.recursive

import java.io.{File, FileInputStream, FileOutputStream, IOException}
import java.util.zip.ZipInputStream

import org.apache.commons.io.FileUtils
import android.content.Context
import android.util.Log
import scodec.bits.BitVector
import Utility.Result

class ZipFileSystem(resId: Int, context: Context) extends FileSystem {
  private val root = context.getExternalFilesDir(null)
  private val flag = new File(root, "suc.res")
  init()
  private def init(): Unit = {
    if (flag.exists)
      return
    FileUtils.deleteQuietly(new File(root, "res"))
    val zs = new ZipInputStream(context.getResources.openRawResource(resId))
    var ze = zs.getNextEntry
    val buffer = Array.ofDim[Byte](1024)
    while (ze != null) {
      val entry = new File(root, ze.getName)
      if (ze.isDirectory)
        entry.mkdir()
      else {
        var os: FileOutputStream = null
        try {
          os = new FileOutputStream(entry)
          var len = zs.read(buffer)
          while (len >= 0) {
            os.write(buffer, 0, len)
            len = zs.read(buffer)
          }
        } catch {
          case e: IOException => Log.e("ZFS", e.getMessage)
        }
        if (os != null)
          os.close()
      }
      ze = zs.getNextEntry
    }
    FileUtils.touch(flag)
  }

  def findFile(path: String): Result[BitVector] = {
    val mpq = "MPQ:(.+)$".r
    val realFile =
      mpq.findPrefixMatchOf(path).map(m => {
        //val path = FilenameUtils.separatorsToUnix(m.subgroups.head.toLowerCase())
        val path: String = ("res" :: m.subgroups.head.toLowerCase().split('\\').toList).mkString("//")
        new File(root, path)
      })
    realFile match {
      case None =>
        Result.fail("File " + path + " cannot be parsed")
      case Some(file) =>
        if (file.exists && !file.isDirectory)
          Result.just(BitVector.fromInputStream(new FileInputStream(file)))
        else
          Result.fail("File " + path + " not found")
    }
  }
}