package org.recursive

import scodec.bits.BitVector
import scodec.{Attempt, DecodeResult, Decoder}
import scodec.codecs._
import shapeless.syntax.SizedConv
import shapeless.{Nat, Sized}
import shapeless.nat._

import scala.reflect.ClassTag

object Utility {
  type Result[T] = Attempt[T]
  object Result {
    def just[T](x: T): Result[T] = {
      Attempt.successful(x)
    }
    def fail[T](e: String): Result[T] = {
      Attempt.failure(new M2DecodeErr(e))
    }
  }
  final case class M2DecodeErr(message: String, context: List[String]) extends scodec.Err {
    def this(message: String) = this(message, Nil)
    def pushContext(ctx: String) = copy(context = ctx :: context)
  }

  implicit class ResultTagMsg[T](val r: Result[T]) extends AnyVal {
    //def !!(msg: String): Result[T] = r.e.left.map(x=>that).right
    def !#(msg: String): Result[T] = r.mapErr(e => e.pushContext(msg))
  }

  type LengthAndOffset = (Long, Long)
  val lengthAndOffset = (uint32L ~ uint32L).as[LengthAndOffset].asDecoder

  def constant[T](v: T) : Decoder[T] = {
    Decoder(b=>Attempt.successful(DecodeResult(v,b)))
  }
  def many[T](n: Long, dec: Decoder[T]): Decoder[List[T]] = {
    if (n==0)
      constant(Nil)
    else for {
      a <- dec
      b <- many(n-1, dec)
    } yield a :: b
  }
  def manyNat[T](n: Nat, dec: Decoder[T])(implicit toInt: shapeless.ops.nat.ToInt[n.N]): Decoder[Sized[List[T],n.N]] = {
    many(toInt(), dec).map(new SizedConv(_).sized(n).get)
  }
  def decodeChunk[T: ClassTag](bv: BitVector, lo: LengthAndOffset, dec: Decoder[T]): Result[Array[T]] = {
    many(lo._1, dec).decode(bv.drop(lo._2*8)).map(_.value.toArray)
  }
  def decodeString(bv: BitVector, lo: LengthAndOffset): Result[String] = {
    bv.drop(lo._2*8).acquire((lo._1-1)*8) match {
      case Left(_)  => Result.fail("")
      case Right(x) => ascii.decode(x).map(_.value)
    }
  }
  type Vector[N <: Nat] = Sized[Array[Float], N]
  type Vector4F = Vector[_4]
  type Vector3F = Vector[_3]
  type Vector2F = Vector[_2]
  object Vector4F {
    def apply(x: Float, y: Float, z: Float, w: Float): Vector4F = {
      Sized.apply[Array].apply(x,y,z,w)
    }
    def apply(v: Vector3F): Vector4F = {
      val u = v.unsized
      Sized.apply[Array].apply(u(0),u(1),u(2),1.0f)
    }
    implicit def vectorOps(vector4F: Vector4F): VectorOps[_4] = new VectorOps(vector4F)
  }
  object Vector3F {
    def apply(x: Float, y: Float, z: Float): Vector3F = {
      Sized.apply[Array].apply(x,y,z)
    }
    implicit def vectorOps(vector3F: Vector3F): VectorOps[_3] = new VectorOps(vector3F)
  }
  object Vector2F {
    def apply(x: Float, y: Float): Vector2F = {
      Sized.apply[Array].apply(x,y)
    }
  }
  class VectorOps[N <: Nat](v: Vector[N]) {
    val negate: Vector[N] = v.map((x: Float) => -x)
    def multiply(s: Float): Vector[N] = v.map((x: Float) => x*s)
    def add(o: Vector[N]): Vector[N] = Sized.wrap((v.unsized zip o.unsized).map(a => a._1 + a._2))
  }
  val vector3F = for {
    a <- floatL.asDecoder
    b <- floatL.asDecoder
    c <- floatL.asDecoder
  }  yield Vector3F(a,b,c)
  val vector2F = for {
    a <- floatL.asDecoder
    b <- floatL.asDecoder
  }  yield Vector2F(a,b)

  type Quaternion = Sized[Array[Float],_4]
  case class PackedQuaternion private()
  object Quaternion {
    def apply(x: Float, y: Float, z: Float, w: Float): Quaternion = {
      Sized.apply[Array].apply(x, y, z, w)
    }
    def apply(x: Int, y: Int, z: Int, w: Int): Quaternion = {
      def pqconv(a: Int): Float = (if (a<0) a + 32768 else a - 32767).toFloat / 32767.0f
      Sized.apply[Array].apply(pqconv(x), pqconv(y), pqconv(z), pqconv(w))
    }
    implicit def quaternionOps(q: Quaternion): QuaternionOps = QuaternionOps(q)
  }

  case class QuaternionOps(q: Quaternion) extends AnyVal {
    def toMatrix: Matrix=>Unit = {
      val uq = q.unsized
      val i = uq(0)
      val j = uq(1)
      val k = uq(2)
      val r = uq(3)
      val i2 = i*i
      val j2 = j*j
      val k2 = k*k
      val r2 = r*r
      val ri = 2*r*i
      val rj = 2*r*j
      val rk = 2*r*k
      val jk = 2*j*k
      val ki = 2*k*i
      val ij = 2*i*j
      // matrix in column-major order
      m => Seq(
        r2+i2-j2-k2, ij+rk,       ki-rj,       0,
        ij-rk,       r2-i2+j2-k2, jk+ri,       0,
        ki+rj,       jk-ri,       r2-i2-j2+k2, 0,
        0,           0,           0,           1)
        .copyToArray(m)
    }
    def dotproduct(q2: Quaternion): Float = {
      (q.unsized zip q2.unsized).map(v=>v._1*v._2).sum
    }
    def normalize: Quaternion = {
      val magSq = dotproduct(q)
      val mag   = Math.sqrt(magSq).toFloat
      multiply(1.0f/mag)
    }
    def multiply(s: Float): Quaternion = {
      val v = q.unsized
      Quaternion(v(0)*s, v(1)*s, v(2)*s, v(3)*s)
    }

    def plus(q2: Quaternion): Quaternion = {
      val v1 = q.unsized
      val v2 = q2.unsized
      Quaternion(v1(0)+v2(0), v1(1)+v2(1), v1(2)+v2(2), v1(3)+v2(3))
    }
  }

  val packedQuaternion = for {
    x <- int16L; y <- int16L; z <- int16L; w <- int16L
  } yield Quaternion(x,y,z,w)

  def sequence[T](t: List[Result[T]]): Result[List[T]] = {
    t.foldRight[Result[List[T]]](Result.just(Nil))((oa,ola) => for {a <- oa; la <- ola} yield a::la )
  }

  type Matrix = Array[Float]
  object Matrix {
    def newM(): Matrix = Array.ofDim(16)
    implicit def matrixOps(m: Matrix): MatrixOps = MatrixOps(m)
  }
  case class MatrixOps(m: Matrix) extends AnyVal {
    def setIdentity(): Matrix = {
      android.opengl.Matrix.setIdentityM(m, 0)
//      for (i <- 0 until 16)
//        m(i) = 0
//      for(i <- 0 until 16 by 5)
//        m(i) = 1.0f
      m
    }
    def translate(v: Vector3F): Matrix = {
      val uv = v.unsized
      android.opengl.Matrix.translateM(m, 0, uv(0), uv(1), uv(2))
//      for (i <- 0 until 4) {
//        m(12 + i) += m(i) * uv(0) + m(4 + i) * uv(1) + m(8 + i) * uv(2)
//      }
      m
    }
    def scale(v: Vector3F): Matrix = {
      val uv = v.unsized
      android.opengl.Matrix.scaleM(m, 0, uv(0), uv(1), uv(2))
//      for (i <- 0 until 4) {
//        m(     i) *= uv(0)
//        m( 4 + i) *= uv(1)
//        m( 8 + i) *= uv(2)
//      }
      m
    }
    def multiply(b: Vector4F): Vector4F  = {
      val r: Array[Float] = Array.ofDim(4)
      android.opengl.Matrix.multiplyMV(r, 0, m, 0, b.unsized, 0)
//      val ub = b.unsized
//      for (i <- 0 until 4)
//        r(i) = m(i)*ub(0) + m(i+4)*ub(1) + m(i+8)*ub(2) + m(i+12)*ub(3)
      Sized.wrap(r)
    }
    def multiply(b: Matrix)(r: Matrix): Unit = {
      android.opengl.Matrix.multiplyMM(r, 0, m, 0, b, 0)
//      for (i <- 0 until 4)
//        for (j <- 0 until 4) {
//          val i4 = i*4
//          r(i4+j) = m(j)*b(i4) + m(j+4)*b(i4+1) + m(j+8)*b(i4+2) + m(j+12)*b(i4+3)
//        }
    }
    def copyFrom(src: Matrix) = {
      src.copyToArray(m)
    }
    def assign(act: Matrix => Unit): Unit = {
      act(m)
    }
    def asString(): String = {
      "4><4 \n" +
      "[%f,%f,%f,%f,\n".format(m(0), m(4), m(8), m(12)) +
      " %f,%f,%f,%f,\n".format(m(1), m(5), m(9), m(13)) +
      " %f,%f,%f,%f,\n".format(m(2), m(6), m(10),m(14)) +
      " %f,%f,%f,%f]"  .format(m(3), m(7), m(11),m(15))
    }
  }
}