package org.recursive

import javax.microedition.khronos.egl.EGLConfig
import javax.microedition.khronos.opengles.GL10
import android.opengl.{GLES10, GLSurfaceView, Matrix}
import gl.GLTexture

class SurfaceRenderer extends GLSurfaceView.Renderer {

  private val projMatrix: Array[Float] = Array.ofDim(16)
  private val viewMatrix: Array[Float] = Array.ofDim(16)
  private var m2Renderer: List[ModelRenderer] = Nil

  def addModelRenderer(modelRenderer: ModelRenderer): Unit = {
    m2Renderer = modelRenderer :: m2Renderer
  }

  override def onSurfaceCreated(gl: GL10, config: EGLConfig): Unit = {
    GLES10.glClearColor(0.4f, 0.4f, 0.4f, 1.0f)
    GLES10.glClearDepthf(1.0f)
    GLES10.glDepthFunc(GLES10.GL_LESS)
    GLES10.glEnable(GLES10.GL_DEPTH_TEST)
    GLES10.glDisable(GLES10.GL_BLEND)
    GLES10.glShadeModel(GLES10.GL_SMOOTH)
    GLES10.glFrontFace(GLES10.GL_CCW)
    GLES10.glCullFace(GLES10.GL_BACK)
    GLES10.glEnable(GLES10.GL_CULL_FACE)
    GLES10.glEnable(GLES10.GL_TEXTURE_2D)
    GLES10.glHint(GLES10.GL_PERSPECTIVE_CORRECTION_HINT, GLES10.GL_NICEST)
    val lightParams = Array(1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.0f, 0.0f, 8.0f, 1.0f)
    GLES10.glLightfv(GLES10.GL_LIGHT1,GLES10.GL_AMBIENT, lightParams,  0)
    GLES10.glLightfv(GLES10.GL_LIGHT1,GLES10.GL_DIFFUSE, lightParams,  4)
    GLES10.glLightfv(GLES10.GL_LIGHT1,GLES10.GL_POSITION, lightParams, 8)
    GLES10.glEnable(GLES10.GL_LIGHT1)
    GLES10.glEnable(GLES10.GL_LIGHT0)
    GLES10.glEnable(GLES10.GL_LIGHTING)
    GLTexture.clear()
  }

  override def onSurfaceChanged(gl: GL10, width: Int, height: Int): Unit = {
    GLES10.glViewport(0, 0, width, height)
    val ratio = width.toFloat / height.toFloat
    GLES10.glMatrixMode(GL10.GL_PROJECTION)
    Matrix.perspectiveM(projMatrix, 0, 45, ratio, 0.1f, 100.0f)
    GLES10.glLoadMatrixf(projMatrix, 0)
    GLES10.glMatrixMode(GLES10.GL_MODELVIEW)
    GLES10.glLoadIdentity()
  }

  override def onDrawFrame(gl: GL10): Unit = {
    gl.glClear(GLES10.GL_COLOR_BUFFER_BIT | GLES10.GL_DEPTH_BUFFER_BIT)
    GLES10.glMatrixMode(GLES10.GL_MODELVIEW)
    Matrix.setLookAtM(viewMatrix, 0, 0.0f, 0.0f, 4.0f, -0.2f, -0.3f, 0.0f, 0.0f, -1.0f, 0.0f)
    GLES10.glLoadMatrixf(viewMatrix, 0)
    GLES10.glRotatef(90, 1.0f, 0.0f, 0.0f)
    m2Renderer.foreach(_.render(viewMatrix, GlobalTime.time()))
  }
}