package org.example

import scodec.bits._
import scodec.codecs._
import shapeless.nat._
import shapeless.{Nat, Sized}
import Utility._

case class Header(id                : ByteVector,
                  version            : ByteVector,
                  globalModelFlags   : Long,
                  views              : Long,
                  physicsSettings    : PhysicsSettings,
                  segments           : Array[LengthAndOffset]) {
  def apply(n: Header.Index.Value): LengthAndOffset = segments(n.id)
  val dump =
    id.toHex + " " + version.toHex + "\n" +
    globalModelFlags.toHexString + "\n" + views.toHexString + "\n" +
    segments.zipWithIndex.map(s => s._2.toString + ":" + s._1._1.toHexString + "," + s._1._2.toHexString).reduce(_ + "\n" + _)
}
object Header {
  object Index extends Enumeration {
    val Name, GlobalSequences, Animation, AnimationLookup, Bone, KeyBoneLookup, Vertex, Color, Texture, Transparency,
    TextureAnimation, TextureReplaceable, TextureFlags, BoneLookup, TexureLookup, TextureUnitLookup,
    TransparencyLookup, TextureAnimationLookup, BoundingTriagle, BoundingVertex, BoundingNormal,
    Attachment, AttachmentLookup, Event, Light, Camera, CameraLookup, RibbonEmitter, ParticleEmitter = Value
  }
  val decoder = for {
    id   <- bytes(4)
    ver  <- bytes(4)
    name <- lengthAndOffset
    flag <- uint32L
    seg1 <- many(6,  lengthAndOffset)
    view <- uint32L
    seg2 <- many(11, lengthAndOffset)
    phys <- PhysicsSettings.decoder
    seg3 <- many(11, lengthAndOffset)
  } yield Header(id, ver, flag, view, phys, (name :: seg1 ::: seg2 ::: seg3).toArray)
}

case class PhysicsSettings(vertexBox1    : Vector3F,
                           vertexBox2     : Vector3F,
                           vertexRadius   : Float,
                           boundingBox1   : Vector3F,
                           boundingBox2   : Vector3F,
                           boundingRadius : Float)
object PhysicsSettings {
  val decoder = for {
    vb1 <- vector3F
    vb2 <- vector3F
    vr  <- floatL
    bb1 <- vector3F
    bb2 <- vector3F
    br  <- floatL
  } yield PhysicsSettings(vb1, vb2, vr, bb1, bb2, br)
}

case class TextureDefinition(typ : Long, flags : Long, name : LengthAndOffset)
object TextureDefinition {
  val decoder = for {
    v1 <- uint32L
    v2 <- uint32L
    v3 <- lengthAndOffset
  } yield TextureDefinition(v1, v2, v3)
}

case class GeosetDefinition(id : Long, vstart: Int, vcount: Int, istart: Int, icount: Int,
                            nbones: Int, startBone: Int, rootBone: Int,
                            boundingBox1: Vector3F, boundingBox2: Vector3F, radius: Float)
object GeosetDefinition {
  val decoder = for {
    v1 <- uint32L
    v2 <- many(8, uint16L)
    v3 <- vector3F
    v4 <- vector3F
    v5 <- floatL
  } yield GeosetDefinition(v1, v2.head, v2(1), v2(2), v2(3), v2(4), v2(5), v2(7), v3, v4, v5)
}

case class RenderPassDefinition(op: Int, flagsIndex: Int, textureId: Int, transparencyId: Int, colorIndex: Int, shading: Int, texunit: Int)
object RenderPassDefinition {
  val decoder = for {
    vs <- many(12, uint16L)
  } yield RenderPassDefinition(vs(2), vs(5), vs(8), vs(10), vs(4), vs(1), vs(6))
}

case class RenderFlag(flags: Int, blend: Int)
object RenderFlag {
  val decoder = for {
    vs <- many(2, uint16L)
  } yield vs match {case List(v0,v1) => RenderFlag(v0,v1)}
}

case class AnimationDefinition (id: Int, subId: Int, length: Long, moveSpeed: Float, loop: Boolean,
                                flags: Long, playSpeed: Long, nextAnim: Int, index: Int) {
  override val toString = {
    val lv: Int = if (loop) 1 else 0
    "Animation: %d-%d. Length:%d. Speed:%.2f. Loop:%d. flags:%d".format(id,subId,length,moveSpeed,lv,flags)
  }
}
object AnimationDefinition {
  val decoder = for {
    v1 <- uint16L
    v2 <- uint16L
    v3 <- uint32L
    v4 <- floatL
    v5 <- many(5, uint32L)   // looptype, flags, unknown, unknown, playspeed
    _  <- many(2, vector3F)  // bounding box
    v7 <- floatL
    v8 <- uint16L
    v9 <- uint16L
  } yield AnimationDefinition(v1,v2,v3,v4,v5.head>0,v5(1),v5(4),v8,v9)
}

case class AnimationBlock(typ: Int, seq: Int, times: LengthAndOffset, keys: LengthAndOffset)
object AnimationBlock {
  val decoder = for {
    v1 <- uint16L
    v2 <- uint16L
    v3 <- lengthAndOffset
    v4 <- lengthAndOffset
  } yield AnimationBlock(v1, v2, v3, v4)
}

case class VertexDefinition(pos: Vector3F, weights: Array[Int], bones: Array[Int], normal: Vector3F, texcoords: Vector2F)
object VertexDefinition {
  private def l2a(a: List[Int]): Sized[Array[Int],_4] = {
    Sized.wrap(a.toArray)
  }
  val decoder = for {
    pos   <- vector3F
    weig  <- many(4, uint8L)
    bone  <- many(4, uint8L)
    norm  <- vector3F
    texc  <- vector2F
    _ <- uint32L
    _ <- uint32L
  } yield VertexDefinition(pos,weig.toArray,bone.toArray,norm,texc)
}

case class BoneDefinition(id: Long, geoset: Long, parent: Long, billboarded: Boolean, pivot: Vector3F,
                          translation: AnimationBlock, scale: AnimationBlock, rotation: AnimationBlock)
object BoneDefinition {
  val decoder = for {
    id   <- uint32L
    flag <- uint32L
    par  <- int16L
    geo  <- uint16L
    _    <- uint32L
    t    <- AnimationBlock.decoder
    r    <- AnimationBlock.decoder
    s    <- AnimationBlock.decoder
    piv  <- vector3F
  } yield BoneDefinition(id, geo, par, flag == 8, piv, t, s, r)
}

case class ViewDefinition(id: Long, indices: LengthAndOffset, triangles: LengthAndOffset, subs: LengthAndOffset, texunits: LengthAndOffset, lod: Long)
object ViewDefinition {
  val decoder = for {
    id <- uint32L
    ind <- lengthAndOffset
    tri <- lengthAndOffset
    pro <- lengthAndOffset
    sub <- lengthAndOffset
    tex <- lengthAndOffset
    lod <- uint32L
  } yield ViewDefinition(id, ind, tri, sub, tex, lod)
}
