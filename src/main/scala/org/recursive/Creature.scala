package org.recursive

import Utility.Result

class Creature(m2model: M2Model, val skins: Array[Creature.Skin], val renderer: ModelRenderer) {
  def setSkin(index: Int): Unit = {
    renderer.setReplaceableTextures(skins(index))
  }
  def setAnim(index: Int): Unit = {
    renderer.setAnimation(index)
  }
}

object Creature {
  // each skin is an array of 14 optional resource path
  type Skin = Array[Option[String]]
  def apply(path: String, fileSystem: FileSystem, database: Database, mkr: M2Model => ModelRenderer): Result[Creature] = {
    val res = "MPQ:" + path + ".m2"
    M2Model.open(fileSystem, res) map { m2 =>
      new Creature(m2,  generateSkinList(path, database), mkr(m2))
    }
  }

  def generateSkinList(path: String, database: Database): Array[Skin] = {
    def matchpath(s: String): Boolean = String.CASE_INSENSITIVE_ORDER.compare(s, path + ".mdx") == 0
    def blppath(s: String): Option[String] = {
      if (s.isEmpty)
        None
      else {
        val cs = path.split('\\')
        cs.update(cs.length-1, s)
        Some("MPQ:" + cs.mkString("\\") + ".blp")
      }
    }
    val ss = database.creatureModelDB.records.toList
      .filter(_.getString(CreatureModelDB.CreatureModelFilename).map(matchpath).getOrElse(false))
      .map { r =>
        if (r.getInt(CreatureModelDB.CreatureModelType).getOrElse(0) != 4) {
          val id = r.getInt(CreatureModelDB.CreatureModelID).getOrElse(0)
          database.creatureSkinDB.records.toList
            .filter(_.getInt(CreatureSkinDB.CreatureSkinModelID).map(_==id).getOrElse(false))
            .map { r =>
              for {
                s0 <- r.getString(CreatureSkinDB.CreatureSkin0)
                s1 <- r.getString(CreatureSkinDB.CreatureSkin1)
                s2 <- r.getString(CreatureSkinDB.CreatureSkin1)
              } yield (s0, s1, s2)
            }
            .filter(_.isSuccessful)
            .map(_.require)
            .toSet
        }
        else
          Set.empty
      }
    (ss.foldLeft(Set.empty[(String,String,String)])(_ ++ _) map { (s: (String,String,String)) =>
      val a: Skin = Array.tabulate(14)(n => None)
      a(11) = blppath(s._1)
      a(12) = blppath(s._2)
      a(13) = blppath(s._3)
      a
    }).toArray
  }
}
