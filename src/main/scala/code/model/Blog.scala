package code.model

import code.lib.Implicits._

import net.liftweb.record.MetaRecord
import net.liftweb.record.Record
import net.liftweb.record.field._
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._

import net.liftweb.common.Box
import TravelLogDB._
import scala.util.Try
import java.util.Calendar

object Blog extends Blog with MetaRecord[Blog] {

  def findByID(id: Long): Box[Blog] = Try {
    inTransaction {
      TravelLogDB.blogs.where(_.id === id).headOption
    }
  }.oToBox

  def add(title: String, description: Box[String], tags: List[String], user: User): Try[Blog] = Try {

    if (title.trim.isEmpty) {
      throw new Exception("請輸入遊記標題")
    }

    val savedBlog = Blog.createRecord.userID(user.id).title(title).description(description).saveTheRecord()
    
    for(blog <- savedBlog; tag <- tags) {
      BlogTag.createRecord.blogID(blog.id).tag(tag).saveTheRecord
    }

    savedBlog.get
  }
}

class Blog extends Record[Blog] with KeyedRecord[Long] {
  override def meta = Blog
  override val idField = new LongField(this)
  
  val userID = new LongField(this, 256)
  val title = new StringField(this, 256)
  val description = new OptionalTextareaField(this, 10000)

  lazy val user = TravelLogDB.userToBlogs.right(this)
  lazy val photos = TravelLogDB.blogToPhotos.left(this)
  lazy val tags = TravelLogDB.blogToTags.left(this)

  def hasNoModified: Try[Boolean] = Try { inTransaction(photos.toList.forall(_.status.get == Photo.Status.Pending)) }
  def isEmpty: Try[Boolean] = Try { inTransaction(photos.toList.isEmpty) }
  def isDone: Try[Boolean] = Try { 
    inTransaction(photos.toList.forall(p => p.status.get == Photo.Status.Done || p.status.get == Photo.Status.Hide)) 
  }

  def blogAuthor: Box[User] = Try(inTransaction { user.headOption }).oToBox
  def coverURL: Box[String] = inTransaction {
    for {
      photos <- orderedPhotos
      url <- photos.find(_.status.get == Photo.Status.Done).map(_.thumbnailOrFullImage)
    } yield url
  }

  def orderedPhotos: Box[List[Photo]] = Try { 
    inTransaction {
      from(photos) { p => select(p) orderBy (p.photoCreatedAt, p.id) }.toList
    }
  }.toBox

  def lastUpdated: Box[Calendar] = for {
    photos <- orderedPhotos
    lastPhoto <- photos.filter(_.status.get == Photo.Status.Done).lastOption
  } yield lastPhoto.lastUpdated.get


  def updateBlog(title: String, description: Box[String], tags: List[String]) = inTransaction {
    this.tags.deleteAll
    tags.foreach { tag => BlogTag.createRecord.blogID(this.id).tag(tag).saveTheRecord }
    this.title(title).description(description).saveTheRecord()
  }

  def delete() {
    inTransaction {
      tags.deleteAll
      photos.foreach(_.caches.deleteAll)
      photos.deleteAll
      blogs.deleteWhere(_.idField === this.idField)
    }
  }

  override def saveTheRecord: Box[Blog] = {
    this.isPersisted match {
      case true  => doAction(this.update, this)
      case false => doAction(this.save, this)
    }
  }
}
