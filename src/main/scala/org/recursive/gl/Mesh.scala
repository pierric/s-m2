package org.recursive.gl

import java.nio.{ByteBuffer, ByteOrder}

import android.opengl.GLES10
import org.recursive._
import Utility._
import android.util.Log

case class SubMesh(vstart: Int, vcount: Int, istart: Int, icount: Int)
case class BoneWeight(index: Int, weight: Int)

class Mesh(m2Model: M2Model) {
  val vertices     = m2Model.vertices
  val positions: Seq[Utility.Vector3F]   = vertices.map(_.pos)
  val boneWeight: Seq[Array[BoneWeight]] = vertices.map(vd => (vd.bones zip vd.weights).map(bw=>BoneWeight(bw._1, bw._2)))
  val indices      = m2Model.indices
  val renderpasses = m2Model.renderPasses
  val subMeshes    = m2Model.geosets.map(gd => SubMesh(gd.vstart, gd.vcount, gd.istart, gd.icount))
  val positionsBuffer = ByteBuffer.allocateDirect(vertices.length * 16).order(ByteOrder.nativeOrder()).asFloatBuffer()
  val normalsBuffer   = ByteBuffer.allocateDirect(vertices.length * 12).order(ByteOrder.nativeOrder()).asFloatBuffer()
  val texcoordsBuffer = ByteBuffer.allocateDirect(vertices.length * 8) .order(ByteOrder.nativeOrder()).asFloatBuffer()
  val indicesBuffer   = ByteBuffer.allocateDirect(indices.length * 2)  .order(ByteOrder.nativeOrder()).asShortBuffer()
  init()

  def init() {
    for(vert <- vertices) {
      positionsBuffer.put(vert.pos.unsized)
      positionsBuffer.put(1.0f)
      normalsBuffer.put(vert.normal.unsized)
      texcoordsBuffer.put(vert.texcoords.unsized)
    }
    indicesBuffer.put(indices.map(_.toShort))
    positionsBuffer.rewind()
    indicesBuffer.rewind()
    normalsBuffer.rewind()
    texcoordsBuffer.rewind()
  }

  def drawSub(index: Int): Unit = {
    val sm = subMeshes(index)
    GLES10.glEnableClientState(GLES10.GL_VERTEX_ARRAY)
    GLES10.glVertexPointer(4,GLES10.GL_FLOAT,0, positionsBuffer)
    GLES10.glEnableClientState(GLES10.GL_NORMAL_ARRAY)
    GLES10.glNormalPointer(GLES10.GL_FLOAT,0,normalsBuffer)
    GLES10.glEnableClientState(GLES10.GL_TEXTURE_COORD_ARRAY)
    GLES10.glTexCoordPointer(2,GLES10.GL_FLOAT,0,texcoordsBuffer)
    GLES10.glDrawElements(GLES10.GL_TRIANGLES, sm.icount, GLES10.GL_UNSIGNED_SHORT, indicesBuffer)
    GLES10.glDisableClientState(GLES10.GL_VERTEX_ARRAY)
    GLES10.glDisableClientState(GLES10.GL_NORMAL_ARRAY)
    GLES10.glDisableClientState(GLES10.GL_TEXTURE_COORD_ARRAY)
  }

  def applyS(skeleton: Array[Matrix]): Unit = {
    def weightedProduct(v: Vector3F, w: Array[BoneWeight]): Vector4F = {
      import Matrix.matrixOps
      import Vector4F.vectorOps
      val v4 = Vector4F(v)
      ((0 to 3) map { i =>
        val bw: BoneWeight = w(i)
        skeleton(bw.index).multiply(v4).multiply(bw.weight.toFloat / 255.0f)
      }).reduce(_ add _)
    }
    for (vert <- (positions zip boneWeight).map(Function.tupled(weightedProduct))) {
      positionsBuffer.put(vert.unsized)
    }
    positionsBuffer.rewind()
  }
}

object Mesh {
  val BUFFER_POSITION = 0
  val BUFFER_NORMAL   = 1
  val BUFFER_TEXCOORD = 2
}