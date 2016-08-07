import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import org.scalatest.{FlatSpec, Matchers}
import org.example._

class T2 extends FlatSpec with Matchers {
  val mpq = MockMPQ("src/test/res/res")

  val blpres = "MPQ:creature\\cat\\white.blp"
  val dxt1 = "MPQ:spells\\cyanstarflash.blp"
  val dxt3 = "MPQ:creature\\cat\\bombay.blp"
  val dxt5 = "MPQ:creature\\cat\\birman.blp"

  "BLP" should "have a valid header" in {
    val r = BLP.open(mpq, blpres)
    assert(r.isSuccessful)
    info(r.require.toString)
  }
//  "DDSDecodeColorBlock" should "decode correct first 2 colors" in {
//    val bv = hex"513b212055414955".toBitVector
//    val cb = DXT.getDdsColorBlock(bv)
//    val cs = DXT.getColorBlockColors(cb)
//    assert(cs(0).toInt == 0x396b8cff)
//    assert(cs(1).toInt == 0x210408ff)
//    assert(cs(2).toInt == 0x314860ff)
//    assert(cs(3).toInt == 0x292634ff)
//  }
//  "DDSDecodeColorBlock" should "decode correct last 2 colors" in {
//    val bv = hex"7e8621205555575c".toBitVector
//    val cb = DXT.getDdsColorBlock(bv)
//    val cs = DXT.getColorBlockColors(cb)
//    assert(cs(2).toInt == 0x638aa7ff)
//    assert(cs(3).toInt == 0x424757ff)
//  }
  "DXT1" should "decode" in {
    val r = BLP.open(mpq, dxt1)
    assert(r.isSuccessful)
    val blp = r.require
    blp match {
      case BLPp(_, _, _, _, _) => fail("NOT DXT1")
      case BLPc(typ, width, height, raws) =>
        assert(typ == CTYPE.DXT1)
        for ((r, i) <- raws.zipWithIndex) {
          val w = (width >> i).toInt
          val h = (height >> i).toInt
          // DXT1 decoding is only meaningful for size >= 4
          if (w >= 4 && h >= 4) {
            val a = DXT.DDSDecompressDXT1(r, w, h)
            val fn = "src\\test\\res\\DXT1-%dx%d".format(w, h)
            info("To compare with " + fn)
            val rfc = new FileInputStream(new File(fn)).getChannel
            assert(4 * w * h == rfc.size)
            val b = ByteBuffer.allocate(rfc.size.toInt)
            rfc.read(b)
            val wfc = new FileOutputStream(new File("src\\test\\res\\N-%dx%d".format(w, h))).getChannel
            wfc.write(a)
            rfc.close()
            wfc.close()
            a.rewind()
            b.rewind()
            assert((a compareTo b) == 0)
          }
        }
    }
  }
  "DXT3" should "decode" in {
    val r = BLP.open(mpq, dxt3)
    assert(r.isSuccessful)
    val blp = r.require
    blp match {
      case BLPp(_, _, _, _, _) => fail("NOT DXT3")
      case BLPc(typ, width, height, raws) =>
        assert(typ == CTYPE.DXT3)
        for ((r, i) <- raws.zipWithIndex) {
          val w = (width >> i).toInt
          val h = (height >> i).toInt
          // DXT3 decoding is only meaningful for size >= 4
          if (w >= 4 && h >= 4) {
            val a = DXT.DDSDecompressDXT3(r, w, h)
            val fn = "src\\test\\res\\DXT3-%dx%d".format(w, h)
            info("To compare with " + fn)
            val rfc = new FileInputStream(new File(fn)).getChannel
            assert(4 * w * h == rfc.size)
            val b = ByteBuffer.allocate(rfc.size.toInt)
            rfc.read(b)
            val wfc = new FileOutputStream(new File("src\\test\\res\\N-%dx%d".format(w, h))).getChannel
            wfc.write(a)
            rfc.close()
            wfc.close()
            a.rewind()
            b.rewind()
            assert((a compareTo b) == 0)
          }
        }
    }
  }
//  "DXT5" should "have correct alphas" in {
//    val bv = hex"bdfe499224449264".toBitVector
//    val DXT.DdsAlphaBlockLinear(as,rs) = DXT.getDdsAlphaBlockLinear(bv)
//    info(as.map("%04x,".format(_)).reduce(_+_))
//  }
  "DXT5" should "decode" in {
    val r = BLP.open(mpq, dxt5)
    assert(r.isSuccessful)
    val blp = r.require
    blp match {
      case BLPp(_, _, _, _, _) => fail("NOT DXT5")
      case BLPc(typ, width, height, raws) =>
        assert(typ == CTYPE.DXT5)
        for ((r, i) <- raws.zipWithIndex) {
          val w = (width >> i).toInt
          val h = (height >> i).toInt
//          new FileOutputStream(new File("src\\test\\res\\R-%dx%d".format(w, h))).getChannel.write(r.toByteBuffer)
//           DXT5 decoding is only meaningful for size >= 4
          if (w >= 4 && h >= 4) {
            val a = DXT.DDSDecompressDXT5(r, w, h)
            val fn = "src\\test\\res\\DXT5-%dx%d".format(w, h)
            info("To compare with " + fn)
            val rfc = new FileInputStream(new File(fn)).getChannel
            assert(4 * w * h == rfc.size)
            val b = ByteBuffer.allocate(rfc.size.toInt)
            rfc.read(b)
            val wfc = new FileOutputStream(new File("src\\test\\res\\N-%dx%d".format(w, h))).getChannel
            wfc.write(a)
            rfc.close()
            wfc.close()
            a.rewind()
            b.rewind()
            assert((a compareTo b) == 0)
          }
        }
    }
  }

  "BLP" should "looks good" in {
    saveDXT("MPQ:creature\\cat\\SilverTabby.blp", "src\\test\\res\\SilverTabby.png")
    saveDXT("MPQ:creature\\cat\\birman.blp", "src\\test\\res\\birman.png")
  }

  def saveDXT(path: String, save: String) = {
    val r = BLP.open(mpq, path)
    assert(r.isSuccessful)
    val blp = r.require
    blp match {
      case BLPp(_, _, _, _, _) => fail("NOT DXT")
      case BLPc(typ, width, height, raws) =>
        val w = width.toInt
        val h = height.toInt
        val pixels = typ match {
          case CTYPE.DXT1 => DXT.DDSDecompressDXT1(raws(0), w, h)
          case CTYPE.DXT3 => DXT.DDSDecompressDXT3(raws(0), w, h)
          case CTYPE.DXT5 => DXT.DDSDecompressDXT5(raws(0), w, h)
        }
        val img = new BufferedImage(w, h, BufferedImage.TYPE_4BYTE_ABGR)
        val arr: Array[Int] = Array.ofDim(w * h)
        pixels.asIntBuffer().get(arr)
        img.setRGB(0,0,w,h,arr,0,w)
        assert(ImageIO.write(img, "png", new File(save)))
    }
  }
}