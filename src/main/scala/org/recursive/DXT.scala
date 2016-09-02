package org.recursive

import java.nio.ByteBuffer

import scodec.bits._
import scodec.codecs._
import Utility.many

object DXT {

  def DDSDecompressDXT1(src: BitVector, width: Int, height: Int): ByteBuffer = {
    DDSDecompressMain(src, width, height, 8, 0, bs => DdsAlphaBlockNone())
  }

  def DDSDecompressDXT3(src: BitVector, width: Int, height: Int): ByteBuffer = {
    DDSDecompressMain(src, width, height, 16, 8, getDdsAlphaBlockExplicit)
  }

  def DDSDecompressDXT5(src: BitVector, width: Int, height: Int): ByteBuffer = {
    DDSDecompressMain(src, width, height, 16, 8, getDdsAlphaBlockLinear)
  }

  private def DDSDecompressMain(src: BitVector, width: Int, height: Int, stride: Int, offset: Int, ma: (=>BitVector)=>DdsAlphaBlock): ByteBuffer = {
    val xBlocks = width  / 4
    val yBlocks = height / 4
    val dest = ByteBuffer.allocate(4*width*height)
    for (y <- 0 until yBlocks) {
      for (x <- 0 until xBlocks) {
        val base = (y * xBlocks + x) * stride
        val cb = getDdsColorBlock(src.drop((base + offset) * 8))
        val cs = getColorBlockColors(cb)
        val ab = ma(src.drop(base * 8))
        dest.position(x * 16 + (y * 4) * width * 4 )
        decodeColorBlock(dest, cb, cs, ab, width)
      }
    }
    dest.rewind(); dest
  }


  private case class DdsColorBlock(colors: Array[BitVector],rows: Array[BitVector])
  private abstract class DdsAlphaBlock
  private case class DdsAlphaBlockNone() extends DdsAlphaBlock
  private case class DdsAlphaBlockExplicit
  (
    row: Array[BitVector] // 4 short int, each contain 4 4-bit alphas.
  ) extends DdsAlphaBlock
  private case class DdsAlphaBlockLinear
  (
    alpha: Array[Int],    // 8 alpha values
    row: Array[Int]       // 16 indices
  ) extends DdsAlphaBlock
  private case class Color(r:Int, g:Int, b:Int, a:Int) {
    def setAlpha(newa: Int): Color = Color(r, g, b, newa)
    val toInt: Int = (r&0xFF) << 24 | (g&0xFF) << 16 | (b&0xFF) << 8 | (a&0xFF)
  }

  private def sliceN(from: BitVector, count: Long, length: Long): (List[BitVector], BitVector) = {
    if (count <= 0)
      (Nil, from)
    else {
      val (a, r) = from.splitAt(length)
      val (as,s) = sliceN(r, count-1, length)
      (a::as, s)
    }
  }
  private def getDdsColorBlock(from: BitVector): DdsColorBlock = {
    val (cs, rest) = sliceN(from, 2, 16)
    val (rows, _) = sliceN(rest, 4, 8)
    DdsColorBlock(cs.toArray,rows.toArray)
  }
  private def getDdsAlphaBlockExplicit(from: =>BitVector): DdsAlphaBlockExplicit = {
    val (rows, _) = sliceN(from, 4, 16)
    DdsAlphaBlockExplicit(rows.toArray)
  }
  private def getDdsAlphaBlockLinear(from: =>BitVector): DdsAlphaBlockLinear = {
    val (abits, rest) = sliceN(from, 2, 8)
    val (List(b0,b1,b2,b3,b4,b5), _) = sliceN(rest, 6, 8)
    val a0 = abits.head.toInt(signed = false)
    val a1 = abits.tail.head.toInt(signed = false)
    val as = Array.ofDim[Int](8)
    as(0) = a0
    as(1) = a1
    if (a0 > a1) {
      for (i <- 2 to 7)
        as(i) = ((8-i)*a0 + (i-1)*a1) / 7
    }
    else {
      for (i <- 2 to 5)
        as(i) = ((6-i)*a0 + (i-1)*a1) / 5
      as(6) = 0
      as(7) = 0xFF
    }
    val rs = (sliceN(b2++b1++b0, 8, 3)._1.reverse ::: sliceN(b5++b4++b3, 8, 3)._1.reverse).map(_.toInt(signed = false)).toArray
    DdsAlphaBlockLinear(as, rs)
  }

  private def getColorBlockColors(cb: DdsColorBlock): Array[Color] = {
    def decodeColor(c: BitVector): Color = {
      val b = c.drop(3).take(5)           toByte false
      val g = c.takeRight(3) ++ c.take(3) toByte false
      val r = c.drop(8).take(5)           toByte false
      Color((r<<3)|(r>>2),(g<<2)|(g>>3),(b<<3)|(b>>2),0xff)
    }
    def average(c1:Int, c2:Int, w:(Int,Int)): Int = {
      (c1*w._1 + c2*w._2) / (w._1 + w._2)
    }
    val c0 = decodeColor(cb.colors(0))
    val c1 = decodeColor(cb.colors(1))
    if (cb.colors(0).toInt(signed = false, ByteOrdering.LittleEndian) >
        cb.colors(1).toInt(signed = false, ByteOrdering.LittleEndian)) {
      val a21 = (2,1)
      val a12 = (1,2)
      val c2 = Color(average(c0.r,c1.r,a21), average(c0.g,c1.g,a21), average(c0.b,c1.b,a21), 0xff)
      val c3 = Color(average(c0.r,c1.r,a12), average(c0.g,c1.g,a12), average(c0.b,c1.b,a12), 0xff)
      Array(c0,c1,c2,c3)
    }
    else {
      val a11 = (1,1)
      val c2 = Color(average(c0.r,c1.r,a11), average(c0.g,c1.g,a11), average(c0.b,c1.b,a11), 0xff)
      val c3 = Color(0, 0xff, 0xff, 0)
      Array(c0,c1,c2,c3)
    }
  }

  private def decodeColorBlock(dest: ByteBuffer, cb: DdsColorBlock, cs: Array[Color], ab: DdsAlphaBlock, width: Int): Unit = {
    val cmasks  = Array(bin"00000011", bin"00001100", bin"00110000", bin"11000000")
    val amasks  = Array(hex"0F00".toBitVector, hex"F000".toBitVector, hex"000F".toBitVector, hex"00F0".toBitVector)
    val cshifts = Array(0, 2, 4, 6)
    val ashifts = Array(8,12, 0, 4)
    for (r <- 0 until 4) {
      for (n <- 0 until 4) {
        val bits = cb.rows(r) and cmasks(n) shiftRight (cshifts(n), signExtension = false) toByte(signed = false)
        if (bits < 4) {
          val a = ab match {
            case DdsAlphaBlockNone() => 0xFF
            case DdsAlphaBlockExplicit(rows) =>
              val p = rows(r) and amasks(n) shiftRight(ashifts(n), signExtension = false) takeRight 4
              (p ++ p).toInt(signed = false)
            case DdsAlphaBlockLinear(as,ss) =>
              as(ss(r*4+n))
          }
          dest.putInt(cs(bits).setAlpha(a).toInt)
        }
        else {
          // something should not happen
          dest.putInt(0)
        }
      }
      // don't go beyond the end of byte buffer
      // only switch to next line if r < 3
      if (r<3)
        dest.position(dest.position()+(width-4)*4)
    }
  }
}