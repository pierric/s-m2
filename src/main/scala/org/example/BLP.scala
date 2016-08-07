package org.example

import java.nio.ByteBuffer

import scodec.codecs._
import scodec.bits._
import Utility._
import scodec.DecodeResult

abstract class BLP
case class BLPp(palette: Array[Long], width: Long, height: Long, alphaBits: Int, raw: Array[BitVector]) extends BLP {
  override def toString: String = "BLPp %d x %d".format(width, height)
}
case class BLPc(typ: CTYPE.Value, width: Long, height: Long, raw: Array[BitVector]) extends BLP {
  override def toString: String = "BLPc %d x %d - %s".format(width, height,typ.toString)
}

object CTYPE extends Enumeration {
  val DXT1, DXT3, DXT5 = Value
}

case class BlpHeader
(
  id: ByteVector,
  typ: Long,
  attr: (Int,Int,Int),
  mipmap: Boolean,
  width: Long,
  height: Long,
  offsets: Array[Long],
  sizes: Array[Long]
)
object BlpHeader {
  val decoder = for {
    id  <- bytes(4)
    typ <- uint32L
    a0  <- uint8L
    a1  <- uint8L
    a2  <- uint8L
    mm  <- uint8L
    w   <- uint32L
    h   <- uint32L
    os  <- many(16,uint32L)
    ss  <- many(16,uint32L)
  } yield {
    assert(id == hex"424c5032")  // BLP2
    BlpHeader(id,typ, (a0,a1,a2), mm > 0, w, h, os.toArray, ss.toArray)
  }
}

object BLP {
  def open(fs: FileSystem, path: String): Result[BLP] = {
    fs.findFile(path) flatMap (archive =>
      BlpHeader.decoder.decode(archive) flatMap ((r:DecodeResult[BlpHeader]) => {
        val hdr = r.value
        val palette = many(256, uint32L).decode(archive.drop(148 * 8))
        val dat = hdr.offsets zip hdr.sizes filter (a => a._1 > 0 && a._2 > 0) map (a => archive.drop(a._1*8).take(a._2*8))
        if (hdr.attr._1 == 1)
          palette.flatMap((r:DecodeResult[List[Long]]) => Result.just(BLPp(r.value.toArray, hdr.width, hdr.height, hdr.attr._2, dat)))
        else
          hdr.attr match {
            case (2,0,_) => Result.just(BLPc(CTYPE.DXT1, hdr.width, hdr.height, dat))
            case (2,1,_) => Result.just(BLPc(CTYPE.DXT1, hdr.width, hdr.height, dat))
            case (2,4,1) => Result.just(BLPc(CTYPE.DXT3, hdr.width, hdr.height, dat))
            case (2,8,1) => Result.just(BLPc(CTYPE.DXT3, hdr.width, hdr.height, dat))
            case (2,8,7) => Result.just(BLPc(CTYPE.DXT5, hdr.width, hdr.height, dat))
            case _ => Result.fail("Unsupported BLP format")
          }
      }))
  }
}

