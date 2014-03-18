package code.model

import code.lib.Implicits._
import code.lib.Session._

import net.liftweb.record.MetaRecord
import net.liftweb.record.Record
import net.liftweb.record.field._
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.common._
import net.liftweb.util.Helpers.{md5, hexEncode}
import scala.util.Try

import TravelLogDB._
import net.liftweb.http.S

object User extends User with MetaRecord[User] {

  object LoginService extends Enumeration {
    type LoginService = Value
    val Google = Value
    val Flickr = Value
    val ImgUr = Value
  }

  def isUsernameTaken(username: String): Try[Boolean] = Try {
    inTransaction {
      users.where(user => user.username === username).headOption.isDefined
    }
  }

  def findByEmail(email: String, service: LoginService.LoginService): Try[Option[User]] = Try {
    inTransaction {
      users.where(user => user.email === email and user.service === service).headOption
    }
  }

  def findByUsername(username: String): Box[User] = Try {
    inTransaction { users.where(_.username === username).headOption }
  }.oToBox

  def findByID(id: Long): Box[User] = Try {
    inTransaction {
      users.where(user => user.id === id).headOption
    }
  }.oToBox

  def isLoggedIn = CurrentUser.is.isDefined
  def currentUser = CurrentUser.is

}

class User extends Record[User] with KeyedRecord[Long] {

  override def meta = User
  override val idField = new LongField(this)

  val email = new EmailField(this, 256)
  val service = new EnumField(this, User.LoginService)

  val nickname = new StringField(this, 256)
  val logtitle = new StringField(this, 256)
  val username = new StringField(this, 256)

  lazy val blogs = TravelLogDB.userToBlogs.left(this)
  lazy val comments = TravelLogDB.userToComments.left(this)

  override def saveTheRecord: Box[User] = {
    this.isPersisted match {
      case true  => doAction(this.update, this)
      case false => doAction(this.save, this)
    }
  }

  def avatarURL = {
    val hash = hexEncode(md5(email.get.toLowerCase.getBytes))
    s"http://www.gravatar.com/avatar/${hash}?d=mm"
  }

  def tags: Box[List[String]] = Try {
    inTransaction {
      for {
        blog <- blogs.toList
        blogTag <- blog.tags
      } yield blogTag.tag.get
    }
  }.toBox

  def isCurrentUser = CurrentUser.is.map(_.username.get == this.username.get).getOrElse(false)

  def login() {
    CurrentUser.set(Some(this))
  }

  def logout() {
    for {
      session <- S.session
      httpSession <- session.httpSession
    } { httpSession.terminate }

    S.redirectTo("/")
  }

}
