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

object Comment extends Comment with MetaRecord[Comment]

class Comment extends Record[Comment] with KeyedRecord[Long] {
  override def meta = Comment
  override val idField = new LongField(this)

  val userID = new LongField(this)
  val photoID = new LongField(this)
  val content = new TextareaField(this, 3000)
  val replyTo = new OptionalLongField(this)
  val dateTime = new DateTimeField(this)

  lazy val photo = TravelLogDB.photoToComments.right(this)
  lazy val user = TravelLogDB.userToComments.right(this)

  def delete() {
    inTransaction {
      comments.deleteWhere(_.replyTo === this.idField)
      comments.deleteWhere(_.idField === this.idField)
    }
  }

  override def saveTheRecord: Box[Comment] = {
    this.isPersisted match {
      case true  => doAction(this.update, this)
      case false => doAction(this.save, this)
    }
  }

}

