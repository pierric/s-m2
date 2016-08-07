import org.scalatest.{FlatSpec, Matchers}
import org.example._

class T3 extends FlatSpec with Matchers {
  val fs = MockMPQ("src/test/res/res")
  val db = Database(fs)

  "Database" should "open" in {
    assert(db.isSuccessful)
  }

  "Cat" should "have correct skins" in {
    val path = "creature\\cat\\cat"
    val ss = Creature.generateSkinList(path, db.require)
    assert(ss.size == 9)
    assert(ss.forall(_.take(11).forall(_.isEmpty)))
    assert(ss.forall(_.takeRight(2).forall(_.isEmpty)))
    val names = ss.map(_(11))
    assert(names.forall(_.isDefined))
    assert(names.map(_.get).toSet == Set("MPQ:creature\\cat\\Birman.blp",
                                   "MPQ:creature\\cat\\Bombay.blp",
                                   "MPQ:creature\\cat\\MaineCoon.blp",
                                   "MPQ:creature\\cat\\SilverTabby.blp",
                                   "MPQ:creature\\cat\\OrangeTabby.blp",
                                   "MPQ:creature\\cat\\Salome.blp",
                                   "MPQ:creature\\cat\\White.blp",
                                   "MPQ:creature\\cat\\Cringer.blp",
                                   "MPQ:creature\\cat\\CornishRex.blp"))
  }
}