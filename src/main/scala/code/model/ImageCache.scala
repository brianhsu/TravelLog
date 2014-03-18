package code.model

import TravelLogDB._

import code.lib.Implicits._

import net.liftweb.record.MetaRecord
import net.liftweb.record.Record
import net.liftweb.record.field._
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.common.Box

import scala.util.Try

object ImageCache extends ImageCache with MetaRecord[ImageCache] {

  def findByPhotoID(photoID: Long, width: Int, height: Int): Box[ImageCache] = Try { 
    inTransaction(
      imageCaches.where(c => c.photoID === photoID and c.width === width and c.height === height).headOption
    )
  }.oToBox

}

class ImageCache extends Record[ImageCache] with KeyedRecord[Long] {
  override def meta = ImageCache
  override val idField = new LongField(this)

  val photoID = new LongField(this)
  val width = new IntField(this)
  val height = new IntField(this)
  val image = new BinaryField(this)

  lazy val user = TravelLogDB.photoToImageCaches.right(this)

  override def saveTheRecord: Box[ImageCache] = {
    this.isPersisted match {
      case true  => doAction(this.update, this)
      case false => doAction(this.save, this)
    }
  }

}

