package code.model

import code.lib.Implicits._

import net.liftweb.record.MetaRecord
import net.liftweb.record.Record
import net.liftweb.record.field._
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._

import net.liftweb.common.Box
import scala.util.Try

object BlogTag extends BlogTag with MetaRecord[BlogTag]

class BlogTag extends Record[BlogTag] {
  def meta = BlogTag

  val blogID = new LongField(this)
  val tag = new StringField(this, 100)

  lazy val blog = TravelLogDB.blogToTags.right(this)

  override def saveTheRecord: Box[BlogTag] = Try {
    inTransaction {
      TravelLogDB.blogTags.insert(this)
      this
    }
  }.toBox

}

