package org.example

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}
import javax.microedition.khronos.egl.EGLConfig
import javax.microedition.khronos.opengles.GL10

import android.app.Activity
import android.content.Context
import android.opengl.{GLES10, GLSurfaceView, Matrix}
import android.os.Bundle
import android.util.AttributeSet
import android.view.{MotionEvent, Window, WindowManager}

class MainActivity extends Activity with TypedFindView {
  lazy val textView = findView(TR.text)
  lazy val surfView = findView(TR.surface)

  val threadpool = new ScheduledThreadPoolExecutor(1)

  lazy val m2Filesystem = new ZipFileSystem(R.raw.res, this)
  lazy val m2database   = Database(m2Filesystem)
  lazy val creature     = m2database flatMap { db =>
    Creature("creature\\cat\\cat", m2Filesystem, db, m2 => new gl.GLRenderer(m2Filesystem, m2))
  }

  /** Called when the activity is first created. */
  override def onCreate(savedInstanceState: Bundle): Unit = {
    super.onCreate(savedInstanceState)
    requestWindowFeature(Window.FEATURE_NO_TITLE)
    getWindow.setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN)
    setContentView(R.layout.main)

    creature.toOption.foreach { cr =>
      textView.setText("Loaded")
      surfView.render.setModelRenderer(cr.renderer)
      GlobalTime.init(threadpool, () => surfView.requestRender())
//      GlobalTime.pause()
//      GlobalTime.set(670)
      cr.setAnim(1)
      cr.setSkin(5)
    }
  }
}

class MySurfaceView(context: Context, attr: AttributeSet) extends GLSurfaceView(context, attr) {
  val render = new Renderer
//  cannot set version to 2, because we use fixed pipeline (1.x)
//  setEGLContextClientVersion(2)
  setRenderer(render)
  setRenderMode(GLSurfaceView.RENDERMODE_WHEN_DIRTY)

  override def onTouchEvent(e: MotionEvent): Boolean = {
    false
  }
}

class Renderer extends GLSurfaceView.Renderer {

  private val projMatrix: Array[Float] = Array.ofDim(16)
  private val viewMatrix: Array[Float] = Array.ofDim(16)
  private var m2Renderer: Option[ModelRenderer] = None

  def setModelRenderer(modelRenderer: ModelRenderer): Unit = {
    m2Renderer = Some(modelRenderer)
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
//    gl.glClearColor(0.4f, 0.4f, 0.4f, 1.0f)
    gl.glClear(GLES10.GL_COLOR_BUFFER_BIT | GLES10.GL_DEPTH_BUFFER_BIT)
    GLES10.glMatrixMode(GLES10.GL_MODELVIEW)
    Matrix.setLookAtM(viewMatrix, 0, 0.0f, 0.0f, 4.0f, -0.2f, -0.3f, 0.0f, 0.0f, -1.0f, 0.0f)
    GLES10.glLoadMatrixf(viewMatrix, 0)
    GLES10.glRotatef(90, 1.0f, 0.0f, 0.0f)
    m2Renderer.foreach(_.render(viewMatrix, GlobalTime.time()))
  }
}