package org.recursive

import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import scodec.bits.BitVector
import scodec.codecs._
import Utility._

object GlobalTime {
  private val atime : AtomicInteger = new AtomicInteger(0)
  private val flag : AtomicBoolean = new AtomicBoolean(false)
  def init(tpool: ScheduledThreadPoolExecutor, extra: () => Unit): Unit = {
    val updater = new Runnable {
      override def run(): Unit = {
        if (!flag.get())
          atime.getAndAdd(33)
        extra()
      }
    }
    tpool.scheduleAtFixedRate(updater, 0, 100, TimeUnit.MILLISECONDS)
  }
  def time()  = atime.get()
  def reset() = atime.set(0)
  def set(t: Int) = atime.set(t)
  def pause()     = flag.set(true)
  def resume()    = flag.set(false)
}

trait FileSystem {
  def findFile(path: String): Result[BitVector]
}

trait ModelRenderer {
  def setAnimation(a: Int): Unit
  def setReplaceableTextures(t: Array[Option[String]]): Unit
  def render(viewMatrix: Matrix, time: Int): Unit
}

case class M2Model(globalSeq: Array[Long], renderPasses: Array[RenderPass], textures: Array[Texture],
                   vertices: Array[VertexDefinition], indices: Array[Int], geosets: Array[GeosetDefinition],
                   bones: Array[Bone], anims: Array[AnimationDefinition])

object M2Model {
  def open(fs: FileSystem, path: String) : Result[M2Model] = {
    for {
      archive <- fs.findFile(path)                                                                  !# "find " + path
      header <- Header.decoder.decode(archive).map(_.value)                                         !# "decode header"
      gs <- decodeChunk(archive, header(Header.Index.GlobalSequences), uint32)                      !# "decode global sequence"
      vs <- decodeChunk(archive, header(Header.Index.Vertex), VertexDefinition.decoder)             !# "decode vertices def"
      view <- lod(fs, path)                                                                         !# "decode geosets"
      txdf <- decodeChunk(archive, header(Header.Index.Texture), TextureDefinition.decoder)         !# "decode texture def"
      rflg <- decodeChunk(archive, header(Header.Index.TextureFlags), RenderFlag.decoder)           !# "decode render flags"
      trlk <- decodeChunk(archive, header(Header.Index.TransparencyLookup), uint16L)                !# "decode transparency lookup"
      txlk <- decodeChunk(archive, header(Header.Index.TexureLookup), uint16L)                      !# "decode texture def"
      talk <- decodeChunk(archive, header(Header.Index.AnimationLookup), uint16L)                   !# "decode animation lookup"
      andf <- decodeChunk(archive, header(Header.Index.Animation), AnimationDefinition.decoder)     !# "decode animation def"
      tulk <- decodeChunk(archive, header(Header.Index.TextureUnitLookup), uint16L)                 !# "decode renderpass def"
      bndf <- decodeChunk(archive, header(Header.Index.Bone), BoneDefinition.decoder)               !# "decode skeleton def"
      textures <- sequence(txdf.toList map { tdef =>
                    if (tdef.typ == 0)
                      decodeString(archive, tdef.name).map((path: String) =>
                        TextureFile("MPQ:"+ path, tdef.flags))                                     !# "texture from file " + tdef.name
                    else
                      Result.just(TextureReplaceable(tdef.typ, tdef.flags))                         !# "replaceable texture"
                    }).map(_.toArray[Texture])                                                      !# "make textures"
      rps <- Result.just(view._3.map(renderPass(_, textures, view._2, rflg, trlk, txlk, talk, tulk)))!# "make renderpasses"
      bns <- animations(fs, path, archive, gs, andf, bndf)                                          !# "make animations"
    } yield M2Model(gs, rps.sorted, textures, vs, view._1, view._2, bns, andf)
  }

  private def lod(fs: FileSystem, path: String) : Result[(Array[Int], Array[GeosetDefinition], Array[RenderPassDefinition])] = {
    for {
      archive <- fs.findFile(Resource(path).skinArchive)
      viewdef <- ViewDefinition.decoder.decode(archive).map(_.value)
      idlk    <- decodeChunk(archive, viewdef.indices, uint16L)
      tris    <- decodeChunk(archive, viewdef.triangles, uint16L)
      gsdf    <- decodeChunk(archive, viewdef.subs, GeosetDefinition.decoder)
      rpdf    <- decodeChunk(archive, viewdef.texunits, RenderPassDefinition.decoder)
    } yield (tris.map(idlk(_)), gsdf, rpdf)
  }

  private def renderPass(renderpassDef: RenderPassDefinition, textures: Array[Texture],
                 geosets: Array[GeosetDefinition], flags: Array[RenderFlag],
                 transparencyLookup: Array[Int], textureLookup: Array[Int],
                 texanimLookup: Array[Int], texunitLookup: Array[Int]) : RenderPass = {
    val op     = renderpassDef.op
    val color  = renderpassDef.colorIndex
    val order  = renderpassDef.shading
    val flag   = flags(renderpassDef.flagsIndex)
    val texid  = textureLookup(renderpassDef.textureId)
    val tex    = textures(texid)
    val opa    = transparencyLookup(renderpassDef.transparencyId)
    val blend  = flag.blend
    val p      = geosets(op).boundingBox1.unsized(2)
    val cull   = (flag.flags & 4) == 0 && blend == 0
    val transp = blend > 0 && opa > 0
    val unlit  = (flag.flags & 1) != 0
    val bb     = (flag.flags | 8) != 0
    val nozw   = (flag.flags & 16) != 0
    val swrap  = (tex.flags & 1) != 0
    val twrap  = (tex.flags & 2) != 0
    val envmap = texunitLookup(renderpassDef.texunit) == -1 && bb && blend > 2
    RenderPass(op, tex, -1, color, opa, blend, order, p, envmap, cull, transp, unlit, nozw, bb, swrap, twrap)
  }

  private def animations(fs: FileSystem, path: String, baseArchive: BitVector, globalSeq: Array[Long],
                 animDefs: Array[AnimationDefinition], boneDefs: Array[BoneDefinition]): Result[Array[Bone]] = {
    val files = animDefs.map(d => fs.findFile(Resource(path).animArchive(d.id, d.subId)).toOption)
    val bones = boneDefs.toList.map(b => for {
      t <- Animated.build[Vector3F](b.translation, globalSeq, baseArchive, files)
      r <- Animated.build[PackedQuaternion](b.rotation, globalSeq, baseArchive, files)
      s <- Animated.build[Vector3F](b.scale, globalSeq, baseArchive, files)
    } yield Bone(b.pivot, b.parent, b.billboarded, t, r, s))
    sequence(bones).map(_.toArray)
  }

  case class Resource(base: String) {
    require(base.endsWith(".m2") || base.endsWith(".M2"))
    private val base0 = base.dropRight(3)
    val skinArchive : String = base0 + "00.skin"
    def animArchive(id:Int,sub:Int) : String = base0 + "%04d-%02d.anim".format(id,sub)
  }
}

abstract class Texture {
  val flags: Long
}
case class TextureFile(path: String, flags: Long) extends  Texture
case class TextureReplaceable(typ: Long, flags: Long) extends Texture

case class RenderPass(geoset: Int, texture: Texture, texAnim: Int, color: Int, opacity: Int, blendMode: Int,
                      order: Int, p: Float,
                      useEnvMap: Boolean, cull: Boolean, transparent: Boolean, unlit: Boolean, nozWrite: Boolean,
                      billboarded: Boolean, swrap: Boolean, twrap: Boolean) {
  override def toString: String =
    "RENDERPASS:order=%d,blend mode=%d,p=%f,geoset=%d".format(order,blendMode,p,geoset)
}
object RenderPass {
  implicit val ord: Ordering[RenderPass] = new Ordering[RenderPass] {
    def compare(x: RenderPass, y: RenderPass): Int = {
      val a = x.order.compare(y.order)
      val b = x.blendMode.compare(y.blendMode)
      val c = x.p.compare(y.p)
      if (a <= 0) a
      else if (b <=0 ) b
      else c
    }
  }
}