package org.example.gl

import java.nio.ByteBuffer

import android.opengl.GLES10
import android.util.Log
import org.example._
import scodec.bits.BitVector

class GLTexture(target: GLTexture.Target.Value, blp: BLP) {
  private val tex = Array.ofDim[Int](1)
  GLES10.glGenTextures(1, tex, 0)
  GLES10.glBindTexture(target.id, tex(0))
  blp match {
    case BLPp(pat, width, height, atype, raw) => paletteImage(pat, width.toInt, height.toInt, atype, raw)
    case BLPc(typ, width, height, raw)        => dxtImage(typ, width.toInt, height.toInt, raw)
  }
  GLES10.glTexParameterx(target.id, GLES10.GL_TEXTURE_WRAP_S, GLES10.GL_CLAMP_TO_EDGE)
  GLES10.glTexParameterx(target.id, GLES10.GL_TEXTURE_WRAP_T, GLES10.GL_CLAMP_TO_EDGE)

  def bind(): Unit = GLES10.glBindTexture(target.id, tex(0))

  private def paletteImage(pat: Array[Long], width: Int, height: Int, atype: Int, raw: Array[BitVector]) = {
    def genColor(c: Int, a: Byte): Array[Byte] = {
      Array(((c & 0xFF0000) >> 16).toByte, ((c & 0xFF00) >> 8).toByte, (c & 0xFF).toByte, a)
    }
    val pixels = ByteBuffer.allocate(4*width*height)
    var l = 0; var w = width; var h = height
    for (r <- raw) {
      pixels.rewind()
      val (color, alpha_byte) = r.toByteVector.splitAt(w * h)
      val alpha_bit = alpha_byte.toBitVector
      for (i <- 0 to color.length.toInt) {
        val a: Byte = atype match {
          case 0 => 0xFF.toByte
          case 1 => if (alpha_bit(i)) 0xFF.toByte else 0
          case 8 => alpha_byte(i)
        }
        pixels.put(genColor(pat(color(i)).toInt, a))
      }
      GLES10.glTexImage2D(GLES10.GL_TEXTURE_2D, l, GLES10.GL_RGBA, w, h, 0, GLES10.GL_RGBA, GLES10.GL_UNSIGNED_BYTE, pixels)
      w /= 2; h /= 2; l += 1
    }
  }

  private def dxtImage(typ: CTYPE.Value, width: Int, height: Int, raw: Array[BitVector]) = {
    var l = 0; var w = width; var h = height
//    val r = raw(0)
    for (r <- raw) {
      val pixels = typ match {
        case CTYPE.DXT1 => DXT.DDSDecompressDXT1(r, w, h)
        case CTYPE.DXT3 => DXT.DDSDecompressDXT3(r, w, h)
        case CTYPE.DXT5 => DXT.DDSDecompressDXT5(r, w, h)
      }
      GLES10.glTexImage2D(GLES10.GL_TEXTURE_2D, l, GLES10.GL_RGBA, w, h, 0, GLES10.GL_RGBA, GLES10.GL_UNSIGNED_BYTE, pixels)
      w /= 2; h /= 2; l += 1
    }
  }
}

object GLTexture {
  object Target extends  Enumeration {
    val Texture2D = Value(GLES10.GL_TEXTURE_2D)
  }

  val textures: scala.collection.mutable.Map[String, GLTexture] = scala.collection.mutable.Map.empty
  def findTexture(fs: FileSystem, path: String): Option[GLTexture] = {
    textures.get(path) match {
      case None =>
        Log.d("TMGR", path)
        BLP.open(fs, path).map(blp => new GLTexture(GLTexture.Target.Texture2D, blp)).toOption match {
          case None => None
          case Some(t) =>
            textures.put(path, t)
            Some(t)
        }
      case Some(t) =>
        Some(t)
    }
  }
}