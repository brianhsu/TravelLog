package code.lib

import code.model._
import code.lib.Implicits._

import net.liftweb.common._
import net.liftweb.common.Box._
import net.liftweb.util.Helpers._
import net.liftweb.http.ResponseShortcutException
import net.liftweb.http.S

import scala.util.Try

object URLParam {

  case class WithBlog(user: User, blog: Blog) {
    def toURLParam = List(user.username.get, blog.idField.toString)
  }

  object WithBlog {
  
    def apply(param: List[String]): Box[WithBlog] = {
      for {
        user <- User.findByUsername(param(0))
        blogID <- asLong(param(1))
        blog <- Blog.findByID(blogID)
      } yield WithBlog(user, blog)
    }

    def hasContent(param: List[String]): Box[WithBlog] = {
      for {
        user <- User.findByUsername(param(0))
        blogID <- asLong(param(1))
        blog <- Blog.findByID(blogID)
        isEmpty <- blog.isEmpty.toBox if !isEmpty 
      } yield WithBlog(user, blog)
    }
 
  }

  case class ChapaterInPage(username: String, page: Page, chapater: Page#Chapater) {
    def toURLParam = List(username, page.blog.idField.toString, chapater.currentChapater.toString)
  }

  object ChapaterInPage {
    def apply(param: List[String]): Box[ChapaterInPage] = {

      for {
        user <- User.findByUsername(param(0))
        blogID <- asLong(param(1))
        chapaterNo <- asInt(param(2))
        blog <- Blog.findByID(blogID)
        author <- blog.blogAuthor
        page = Page(blog)
        chapater <- page.chapater(chapaterNo) if author.id == user.id
      } yield ChapaterInPage(author.username.get, page, chapater)

    }
  } 

  case class PageInfo(username: String, page: Page) {
    def toURLParam = List(username, page.blog.idField.toString)
  }

  object PageInfo {
    def apply(param: List[String]): Box[PageInfo] = {
      for {
        user <- User.findByUsername(param(0))
        blogID <- asLong(param(1))
        blog <- Blog.findByID(blogID)
        author <- blog.blogAuthor
        isEmpty = blog.isEmpty
        page = Page(blog) if !isEmpty.getOrElse(false)
      } yield PageInfo(author.username.get, page)
    }
  }

}

