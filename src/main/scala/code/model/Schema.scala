package code.model

import org.squeryl.Schema

import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.common._

import scala.util.Try
import scala.util.Failure
import scala.util.Success

object TravelLogDB extends Schema {
  val users = table[User]
  val blogs = table[Blog]
  val photos = table[Photo]
  val blogTags = table[BlogTag]
  val imageCaches = table[ImageCache]
  val comments = table[Comment]

  val photoToComments = oneToManyRelation(photos, comments).via { (photo, comment) => photo.id === comment.photoID }
  val userToComments = oneToManyRelation(users, comments).via { (user, comment) => user.id === comment.userID }

  val userToBlogs = oneToManyRelation(users, blogs).via { (user, blog) => user.id === blog.userID }
  val photoToImageCaches = oneToManyRelation(photos, imageCaches).via { (photo, cache) => photo.id === cache.photoID }

  val blogToPhotos = oneToManyRelation(blogs, photos).via { (blog, photo) => blog.id === photo.blogID }
  val blogToTags = oneToManyRelation(blogs, blogTags).via { (blog, tag) => blog.id === tag.blogID }

  def doAction[T](action: => Any, result: T): Box[T] = Try(action) match {
    case Success(nothing) => Full(result)
    case Failure(error) => net.liftweb.common.Failure(error.getMessage, Full(error), Empty)
  }

}
