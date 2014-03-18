package code.model

import code.lib.Implicits._

import net.liftweb.record.MetaRecord
import net.liftweb.record.Record
import net.liftweb.record.field._
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Helpers

import org.squeryl.annotations.Column

import net.liftweb.common.Box
import scala.util.Try
import scala.util.Random

import TravelLogDB._

object Photo extends Photo with MetaRecord[Photo] {

  object Status extends Enumeration {
    type Status = Value
    val Pending = Value
    val Photo = Value
    val Done = Value
    val Hide = Value
  }

  def findByIDInService(idInService: String): Box[Photo] = Try { 
    inTransaction(TravelLogDB.photos.where(_.idInService === idInService).headOption)
  }.oToBox

  private def noRepeatRandom(min:Long, max: Long, num: Int): Set[Long] = {
    var randomSet: Set[Long] = Set()

    while (randomSet.size < num && randomSet.size < (max - min)) {
      val random = Helpers.randomLong(max + 1)

      if (random >= min && random <= max) {
        randomSet += random
      }
    }

    randomSet
  }

  def randomList(size: Int): Box[List[Photo]] = Try {
    
    inTransaction {

      val maxID = from(TravelLogDB.photos) {photo => compute(max(photo.id))}.get
      val minID = from(TravelLogDB.photos) {photo => compute(min(photo.id))}.get
      val randomIDs = noRepeatRandom(minID, maxID, size * 10)

      val photoIDList = from(TravelLogDB.photos) { photo =>
        where(photo.id in randomIDs) select(photo)
      }.toList

      Random.shuffle(photoIDList).take(size)
    }

  }.toBox

  def findByShortID(shortID: String): Box[Photo] = Try {
    inTransaction(TravelLogDB.photos.where(_.id === java.lang.Long.parseLong(shortID, 24)).headOption)
  }.oToBox

}

class Photo extends Record[Photo] with KeyedRecord[Long] {
  
  override def meta = Photo

  override val idField = new LongField(this, 1000)

  val blogID = new LongField(this)
  val title = new OptionalStringField(this, 256)
  val chapater = new StringField(this, 256)
  val content = new OptionalTextareaField(this, 10000)
  val photoCreatedAt = new DateTimeField(this)
  val editAt = new OptionalDateTimeField(this)
  val gpsPoint = new OptionalStringField(this, 256)
  val status = new EnumField(this, Photo.Status)
  val photoURL = new StringField(this, 500)
  val thumbnailURL = new OptionalStringField(this, 500)
  val idInService = new StringField(this, 500)
  val lastUpdated = new DateTimeField(this)

  lazy val blog = TravelLogDB.blogToPhotos.right(this)
  lazy val caches = TravelLogDB.photoToImageCaches.left(this)
  lazy val comments = TravelLogDB.photoToComments.left(this)

  def blogAuthor: Option[User] = inTransaction { 
    for {
      blog <- blog.headOption
      blogAuthor <- blog.blogAuthor
    } yield blogAuthor
  }

  def thumbnailOrFullImage = thumbnailURL.get.getOrElse(photoURL.get)

  override def saveTheRecord: Box[Photo] = {
    this.isPersisted match {
      case true  => doAction(this.update, this)
      case false => doAction(this.save, this)
    }
  }

  def shortID = java.lang.Long.toString(id, 24)
  def resizedURL(width: Int, height: Int) = s"/images/cover/${shortID}/${width}/${height}"
}

