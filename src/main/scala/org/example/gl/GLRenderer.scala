package org.example.gl

import java.util.concurrent.atomic.AtomicInteger

import android.opengl.GLES10
import android.util.Log
import org.example._

class GLRenderer(fs: FileSystem, model: M2Model) extends ModelRenderer {
  val mesh = new Mesh(model)
  var textures : Option[Array[Option[String]]] = None
  var anim = -1

  override def setAnimation(a: Int): Unit = {
    anim = a
  }

  override def setReplaceableTextures(t: Array[Option[String]]): Unit = {
    textures = Some(t)
  }

  override def render(viewMatrix: Utility.Matrix, time: Int): Unit = {
    if (anim >= 0) {
      mesh.applyS(Bone.transform(viewMatrix, model.bones, anim, time))
    }
    for (rp <- model.renderPasses) {
      val texpath = rp.texture match {
        case TextureFile(p, _) => Some(p)
        case TextureReplaceable(t, _) => textures.flatMap(_(t.toInt))
      }
//      val texpath = None
      texpath.flatMap(GLTexture.findTexture(fs, _)) match {
        case None =>
          GLES10.glDisable(GLES10.GL_BLEND)
          GLES10.glDisable(GLES10.GL_ALPHA_TEST)
          GLES10.glBindTexture(GLES10.GL_TEXTURE_2D, 0)
          mesh.drawSub(rp.geoset)
        case Some(tobj) =>
          rp.blendMode match {
            case 0 => ;
            case 1 => GLES10.glAlphaFunc(GLES10.GL_EQUAL, 0.7f)
            case 2 =>
              GLES10.glEnable(GLES10.GL_BLEND)
              GLES10.glBlendFunc(GLES10.GL_SRC_ALPHA, GLES10.GL_ONE_MINUS_SRC_ALPHA)
            case 3 =>
              GLES10.glEnable(GLES10.GL_BLEND)
              GLES10.glBlendFunc(GLES10.GL_SRC_COLOR, GLES10.GL_ONE)
            case 4 =>
              GLES10.glEnable(GLES10.GL_BLEND)
              GLES10.glBlendFunc(GLES10.GL_SRC_ALPHA, GLES10.GL_ONE)
            case 5 =>
              GLES10.glEnable(GLES10.GL_BLEND)
              GLES10.glBlendFunc(GLES10.GL_DST_COLOR, GLES10.GL_SRC_COLOR)
            case 6 =>
              GLES10.glEnable(GLES10.GL_BLEND)
              GLES10.glBlendFunc(GLES10.GL_DST_COLOR, GLES10.GL_SRC_COLOR)
          }
          if (rp.nozWrite) GLES10.glDepthMask(false)
          if (rp.unlit) GLES10.glDisable(GLES10.GL_LIGHTING)
          if (rp.cull) GLES10.glDisable(GLES10.GL_CULL_FACE)
          tobj.bind()
          mesh.drawSub(rp.geoset)
          GLES10.glDisable(GLES10.GL_BLEND)
          GLES10.glDisable(GLES10.GL_ALPHA_TEST)
          if (rp.nozWrite) GLES10.glDepthMask(true)
          if (rp.unlit) GLES10.glEnable(GLES10.GL_LIGHTING)
          if (rp.cull) GLES10.glEnable(GLES10.GL_CULL_FACE)
      }
    }
  }
}