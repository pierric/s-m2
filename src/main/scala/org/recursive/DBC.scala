package org.recursive

import Utility.{Result, many}
import scodec.Attempt
import scodec.Attempt.Failure
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{cstring, uint32L}

trait Field {
  val offset: Int
}

trait DBC {
  type F <: Field
  val records: Array[Record[F]]
}

case class Record[F <: Field](raw: BitVector, string: BitVector, base: Long) {
  def getInt(field: F): Result[Int] = {
    uint32L.decodeValue(raw.drop((base+field.offset)*8)).map(_.toInt)
  }

  def getString(field: F): Result[String] = {
    getInt(field).flatMap(offset => cstring.decodeValue(string.drop(offset*8)))
  }
}

object DBC {
  def open(from: BitVector): Result[(Long,Long,BitVector,BitVector)] = {
    many(5, uint32L).decodeValue(from).flatMap( ints => {
      val List(hd,nr,nf,sr,ss) = ints
      if (hd != 1128416343)
        Result.fail("DBC archive corrupt.")
      else {
        val sz = sr*nr*8
        val dat = from.drop(160).take(sz)
        val str = from.drop(160+sz).take(ss*8)
        Result.just((nr,sr,dat,str))
      }
    })
  }
}

object CreatureModelDB extends Enumeration {
  case class F(offset: Int) extends Field
  val CreatureModelID       = F(0)
  val CreatureModelType     = F(4)
  val CreatureModelFilename = F(8)

  def open(from: BitVector): Result[CreatureModelDB] = {
    DBC.open(from).map(Function.tupled(CreatureModelDB.apply))
  }
}
case class CreatureModelDB(rec_num: Long, rec_size: Long, raw: BitVector, string: BitVector) extends DBC {
  type F = CreatureModelDB.F
  val records: Array[Record[F]] = Array.tabulate(rec_num.toInt)((i: Int) => Record(raw, string, i*rec_size))
}

object CreatureSkinDB extends Enumeration {
  case class F(offset: Int) extends Field
  val CreatureSkinID       = F(0)
  val CreatureSkinModelID  = F(4)
  val CreatureSkinNPCID    = F(12)
  val CreatureSkin0        = F(24)
  val CreatureSkin1        = F(28)
  val CreatureSkin2        = F(32)

  def open(from: BitVector): Result[CreatureSkinDB] = {
    DBC.open(from).map(Function.tupled(CreatureSkinDB.apply))
  }
}
case class CreatureSkinDB(rec_num: Long, rec_size: Long, raw: BitVector, string: BitVector) extends DBC {
  type F = CreatureSkinDB.F
  val records: Array[Record[F]] = Array.tabulate(rec_num.toInt)((i: Int) => Record(raw, string, i*rec_size))
}

class Database(val creatureSkinDB: CreatureSkinDB, val creatureModelDB: CreatureModelDB)

object Database {
  val creatureSkinDB  = "MPQ:DBFilesClient\\CreatureDisplayInfo.dbc"
  val creatureModelDB = "MPQ:DBFilesClient\\CreatureModelData.dbc"

  def apply(fileSystem: FileSystem): Result[Database] = {
    val rs = fileSystem.findFile(creatureSkinDB)  flatMap CreatureSkinDB.open
    val rm = fileSystem.findFile(creatureModelDB) flatMap CreatureModelDB.open
    rs flatMap (s => rm flatMap (m => Result.just(new Database(s,m))))
  }
}
