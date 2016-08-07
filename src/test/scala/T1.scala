import java.io.{File, FileInputStream}

import org.example._
import org.example.Utility._
import org.scalatest._
import scodec.bits._

case class MockMPQ(prefix: String) extends FileSystem {
  def findFile(path: String): Result[BitVector] = {
    val mpq   = "MPQ:(.+)$".r
    val local = "FILE:(.+)$".r
    val realPath =
      local.findPrefixMatchOf(path).map(_.subgroups.head).orElse(
        mpq.findPrefixMatchOf(path).map(m => {
          val pathComponents: Array[String] = m.subgroups.head.toLowerCase().split('\\')
          (Array(prefix) ++ pathComponents).mkString("//")
        }))
    realPath match {
      case None =>
        Result.fail("File " + path + " cannot be parsed")
      case Some(filepath) =>
        val file = new File(filepath)
        if (file.exists && !file.isDirectory)
          Result.just(BitVector.fromInputStream(new FileInputStream(file)))
        else
          Result.fail("File " + path + " not found")
    }
  }
}

class T1 extends FlatSpec with Matchers {

  val mpq = MockMPQ("src/test/res/res")
  val m2res = "MPQ:creature\\cat\\cat.m2"

  "MPQ" should "find a m2 resource" in {
    val o = mpq.findFile(m2res)
    assert(o.isSuccessful)
  }

  it should "have a valid header" in {
    val o = mpq.findFile(m2res).require
    val r = Header.decoder.decode(o)
    assert(r.isSuccessful)
    val h = r.require.value
    assert(h.id == hex"4d443230")
    assert(Header.Index.Texture.id == 8)
    val lo = h.segments(8)
    assert(lo == (0x02,0x8ff0))
    //val tc = Utility.decodeChunk(o, h.segments(8), example.TextureDefinition.decoder)
    val tc = many(lo._1, TextureDefinition.decoder).decode(o.drop(lo._2*8)).map(_.value.toArray)
    assert(tc.isSuccessful)
    val t1 = tc.require(0)
    val t2 = tc.require(1)
    assert(t1.typ == 0x0b)
    assert(t1.flags == 0x00)
    assert(t1.name == (0x01, 0x9010))
    assert(t2.typ == 0x00)
    assert(t2.flags == 0x03)
    assert(t2.name == (0x18, 0x9020))
  }

  it should "succeed to open it" in {
    val o = M2Model.open(mpq, m2res)
    assert(o.isSuccessful)
  }

  it should "has valid animation" in {
    val o = M2Model.open(mpq, m2res)
    assert(o.require.anims.length == 27)
    val bone = o.require.bones(0)
    val tr = bone.translation
    val rt = bone.rotation
    val sc = bone.scale
    info("Bone 0 translation: # of anim - " + tr.data.length.toString)
    info("Bone 0 rotation   : # of anim - " + rt.data.length.toString)
    info("Bone 0 scaling    : # of anim - " + sc.data.length.toString)

  }

  it should "has correct bone data" in {
    val o = M2Model.open(mpq, m2res)
    val bone = o.require.bones(0)
    val tr = bone.translation
    val rt = bone.rotation
    val sc = bone.scale
    val dv = Vector3F(0,0,0)
    val dq = Quaternion(0.0f, 0.0f, 0.0f, 0.0f)
    val ds = Vector3F(1,1,1)
    // Vector3 (-0.11800946) (-5.7416666e-5) (-4.442219e-2)
    info("Bone 0 Anim 0 - translation at time 0   " + Interpolatable.ivec3.asString(tr.at(0, 0, dv)))
    // Vector3 (-0.122274905) (-3.5517502e-3) (-4.707277e-2)
    info("Bone 0 Anim 0 - translation at time 200 " + Interpolatable.ivec3.asString(tr.at(0, 200, dv)))
    // Vector3 (-0.12359049) (-4.4196392e-3) (-4.9571056e-2)
    info("Bone 0 Anim 0 - translation at time 300 " + Interpolatable.ivec3.asString(tr.at(0, 300, dv)))
    // Vector3 (-0.116461) 2.3967173e-2 (-8.254766e-2)
    info("Bone 0 Anim 1 - translation at time 250 " + Interpolatable.ivec3.asString(tr.at(1, 250, dv)))

    // all Quaternion(0.0, 0.0, 0.0, 0.0)
    info("Bone 0 Anim 0 - rotation    at time   0 " + Interpolatable.iqua.asString(rt.at(0,   0, dq)))
    info("Bone 0 Anim 0 - rotation    at time 250 " + Interpolatable.iqua.asString(rt.at(0, 250, dq)))
    info("Bone 0 Anim 1 - rotation    at time   0 " + Interpolatable.iqua.asString(rt.at(1,   0, dq)))
    info("Bone 0 Anim 1 - rotation    at time 250 " + Interpolatable.iqua.asString(rt.at(1, 250, dq)))

    // all Vector3(1.0, 1.0, 1.0)
    info("Bone 0 Anim 0 - scale       at time   0 " + Interpolatable.ivec3.asString(sc.at(0,  0, ds)))
    info("Bone 0 Anim 0 - scale       at time 250 " + Interpolatable.ivec3.asString(sc.at(0,250, ds)))
    info("Bone 0 Anim 1 - scale       at time   0 " + Interpolatable.ivec3.asString(sc.at(1,  0, ds)))
    info("Bone 0 Anim 1 - scale       at time 250 " + Interpolatable.ivec3.asString(sc.at(1,250, ds)))

    val bone1 = o.require.bones(1)
    val tr1 = bone1.translation
    val rt1 = bone1.rotation
    val sc1 = bone1.scale
    info("Bone 1 Anim 0 - translation at time   0 " + Interpolatable.ivec3.asString(tr1.at(0, 0, dv)))
    info("Bone 1 Anim 0 - rotation    at time   0 " + Interpolatable.iqua.asString (rt1.at(0, 0, dq)))
    info("Bone 1 Anim 0 - scale       at time   0 " + Interpolatable.ivec3.asString(sc1.at(0, 0, ds)))
  }

  it should "have correct skeleton structure" in {
    val o = M2Model.open(mpq, m2res)
    val bones = o.require.bones
    for (b <- bones.zipWithIndex) {
      info("Bone # " + b._2.toString + " with parent # " + b._1.parent.toString)
    }
  }

  it should "have correct render passes" in {
    val o = M2Model.open(mpq, m2res)
    for (rp <- o.require.renderPasses) {
      info(rp.toString)
    }
  }

//  it should "have correct anim matrix" in {
//    val o = M2Model.open(mpq, m2res)
//    val bones = o.require.bones
//    val iv = Matrix.newM()
//    Matrix.setIdentity(iv)
//    val ms = Bone.transform(iv, bones, 0, 0)
//    for (m <- ms) {
//      info("(4><4)\n" + Matrix.asString(m))
//    }
//  }

  def calc_matrix(p: Vector3F, t: Vector3F, r: Quaternion, s: Vector3F, m: Option[Matrix]): Matrix = {
    import Vector3F.vectorOps
    import Matrix.matrixOps
    import Quaternion.quaternionOps
    val temp0 = Matrix.newM()
    val temp1 = Matrix.newM()
    val temp2 = Matrix.newM()
    m match {
      case None => temp0.setIdentity()
      case Some(pm) => temp0.copyFrom(pm)
    }
    temp0.translate(p)
    temp0.translate(t)
    temp2.assign(r.toMatrix)
    temp1.assign(temp0 multiply temp2)
    temp1.scale(s)
    temp1.translate(p.negate)
    temp1
  }
}
