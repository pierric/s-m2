package org.recursive

import scodec.Decoder
import scodec.bits.BitVector
import scodec.codecs._
import shapeless.Sized
import Utility._

import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

trait KeyDataProp[T] {
  type KT
  val classtag: ClassTag[KT]
  def interpolater: Interpolatable[KT]
  def decoder: Decoder[KT]
}
object KeyDataProp {
  implicit val keyPackedQuat = new KeyDataProp[PackedQuaternion]{
    type KT = Quaternion
    val classtag = classTag[KT]
    val interpolater = Interpolatable.iqua
    val decoder = packedQuaternion
  }
  implicit val keyVector3F = new KeyDataProp[Vector3F] {
    type KT = Vector3F
    val classtag = classTag[KT]
    val interpolater = Interpolatable.ivec3
    val decoder = vector3F
  }
}

trait Interpolatable[T] {
  def lerp(segment: (T,T), ratio: Float): T
  def asString(t: T): String
}
object Interpolatable {
  implicit val ifloat = new Interpolatable[Float] {
    def lerp(segment: (Float,Float), ratio: Float): Float = {
      segment._1 + (segment._2 - segment._1) * ratio
    }
    def asString(t: Float): String = t.toString
  }
  implicit val ivec3 = new Interpolatable[Vector3F] {
    def lerp(segment: (Vector3F,Vector3F), ratio: Float): Vector3F = {
      Sized.wrap((segment._1.unsized zip segment._2.unsized).map(ifloat.lerp(_, ratio)))
    }
    def asString(t: Vector3F): String = t.unsized.toList.map(_.toString).toString
  }
  implicit val iqua = new Interpolatable[Quaternion] {
    def lerp(segment: (Quaternion,Quaternion), ratio: Float): Quaternion = {
      Sized.wrap((segment._1.unsized zip segment._2.unsized).map(ifloat.lerp(_, ratio)))
//      import Quaternion.quaternionOps
//      val nq1 = segment._1.normalize
//      val nq2 = segment._2.normalize
//      val t   = Math.acos(nq1 dotproduct nq2)
//      val st  = Math.sin(t)
//      val a   = (Math.sin((1 - ratio) * t) / st).toFloat
//      val b   = (Math.sin(ratio * t) / st).toFloat
//      val nsq1= nq1 multiply a
//      val nsq2= nq2 multiply b
//      (nsq1 plus nsq2).normalize
    }
    def asString(t: Quaternion): String = t.unsized.toList.map(_.toString).toString
  }
}

abstract class KeyFrame[T] {
  val empty: Boolean
  protected val keys_time: List[Long]
  protected val key_time_max: Long
  protected def interpolate(ratio: Float, pos: Int): T

  final def at(global: Option[Long], time: Long, default: T): T = {
    if (empty) {
      default
    }
    else {
      val timew = global match {
        case None => time % key_time_max
        case Some(0) => 0
        case Some(n) => time % (key_time_max max n)
      }
      (keys_time zip keys_time.tail).zipWithIndex.find(k => k._1._1 <= timew && timew < k._1._2) match {
        case None =>
          default
        case Some((interval,pos)) =>
          val ratio = (timew - interval._1).toFloat / (interval._2 - interval._1).toFloat
          interpolate(ratio, pos)
      }
    }
  }
}
case class KeyFrameLinear[T](keyframes: (Array[Long], Array[T]), ev: Interpolatable[T]) extends KeyFrame[T] {
  val empty = keyframes._1.length == 0
  protected lazy val keys_time = keyframes._1.toList
  protected lazy val key_time_max = keyframes._1.last
  protected lazy val keys_data = keyframes._2
  protected def interpolate(ratio: Float, pos: Int): T = {
    val s = keys_data(pos)
    val e = keys_data(pos+1)
    ev.lerp((s,e),ratio)
  }
}
case class KeyFrameHermite[T](keyframes: (Array[Long], Array[T])) extends KeyFrame[T] {
  require(keyframes._1.length * 3 == keyframes._2.length)
  val empty = keyframes._1.length == 0
  lazy val keys_time = keyframes._1.toList
  lazy val key_time_max = keyframes._1.last
  lazy val keys_data = Array.ofDim(keyframes._1.length)
  lazy val keys_in   = Array.ofDim(keyframes._1.length)
  lazy val keys_out  = Array.ofDim(keyframes._1.length)
  def interpolate(ratio: Float, pos: Int): T = {
    sys.error("KeyFrameHermite: not supported yet")
  }
}
case class Animated[T](global: Option[Long], data: Array[KeyFrame[T]]) {
  def at(index: Int, time: Long, default: T): T = {
    if (index < 0 || index >= data.length)
      default
    else
      data(index).at(global, time, default)
  }
}
object Animated {
  def build[T](animationBlock: AnimationBlock, globalSeq: Array[Long], baseArchive: BitVector,
               animArchive: Array[Option[BitVector]])(implicit ev: KeyDataProp[T]) : Result[Animated[ev.KT]] = {
    var gs: Option[Long] = None
    if (animationBlock.seq < globalSeq.length)
      gs = Some(globalSeq(animationBlock.seq))

    def eachAnim(ht: Array[(Long, Long)], hk: Array[(Long, Long)], i: Int): Result[(Array[Long], Array[ev.KT])] = {
      val from = animArchive(i).getOrElse(baseArchive)
      decodeChunk(from, ht(i), uint32L) flatMap { ts =>
        decodeChunk(from, hk(i), ev.decoder)(ev.classtag) map { ks =>
          (ts, ks)
        }
      }
    }
    assert(animationBlock.typ >=0 && animationBlock.typ <= 2)
    for {
      ht <- decodeChunk(baseArchive, animationBlock.times, uint32L ~ uint32L)
      hk <- decodeChunk(baseArchive, animationBlock.keys, uint32L ~ uint32L)
      keyframes <- sequence(ht.indices.toList.map(eachAnim(ht,hk,_)))
    } yield {
      val kf: Array[KeyFrame[ev.KT]] =
        if (animationBlock.typ <= 1)
          keyframes.map(KeyFrameLinear(_, ev.interpolater)).toArray
        else
          keyframes.map(KeyFrameHermite(_)).toArray
      new Animated(gs, kf)
    }
  }
}

case class Bone(pivot: Vector3F, parent: Long, billboarded: Boolean,
                translation: Animated[Vector3F], rotation: Animated[Quaternion], scale: Animated[Vector3F])
object Bone {
  def transform(viewMatrix: Matrix, bones: Array[Bone], anim: Int, time: Int): Array[Matrix] = {
    import Matrix.matrixOps
    import Vector3F.vectorOps
    import Quaternion.quaternionOps
    val boneWithIndex = bones.zipWithIndex
    val queue : mutable.Queue[Int] = mutable.Queue(-1)
    val matrices: Array[Matrix] = Array.tabulate(bones.length)((i:Int)=>Matrix.newM().setIdentity())
    val temp0: Matrix = Matrix.newM()
    val temp1: Matrix = Matrix.newM()
    val temp2: Matrix = Matrix.newM()
    while (queue.nonEmpty) {
      val last = queue.dequeue()
      for (bone <- boneWithIndex; if bone._1.parent == last) {
        val p = bone._1.pivot
        val t = bone._1.translation.at(anim, time, Vector3F(0,0,0))
        val r = bone._1.rotation.at(anim, time, Quaternion(0.0f,0.0f,0.0f,1.0f))
        val s = bone._1.scale.at(anim, time, Vector3F(1,1,1))
        temp2.assign(r.toMatrix)
        if (last < 0)
          temp0.setIdentity()
        else
          temp0.copyFrom(matrices(last))
        temp0
          .translate(p)
          .translate(t)
        temp1.assign(temp0 multiply temp2)
        temp1
          .scale(s)
          .translate(p.negate)
        matrices(bone._2).copyFrom(temp1)

        queue.enqueue(bone._2)
      }
    }
    matrices
  }
}