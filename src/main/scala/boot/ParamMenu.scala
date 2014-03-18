package bootstrap.liftweb

import code.model._
import code.lib.Implicits._
import code.lib.URLParam._

import net.liftweb.sitemap.Loc.Template
import net.liftweb.sitemap._
import net.liftweb.http.Templates

import scala.xml.NodeSeq

object ParamMenu {

  object AddBlog {
    val menu = Menu.param[User]("AddBlog", "AddBlog", s => User.findByUsername(s), user => user.username.get)
    val template = Template(() => Templates("log" :: "new" :: Nil) openOr NodeSeq.Empty)
  }

  object EditBlog {
    val template = Template(() => Templates("log" :: "edit" :: Nil) openOr NodeSeq.Empty)
    val menu = Menu.params[WithBlog]("EditBlog", "EditBlog", WithBlog.apply _, _.toURLParam)
  }

  object ComposeBlog {
    val template = Template(() => Templates("log" :: "compose" :: Nil) openOr NodeSeq.Empty)
    val menu = Menu.params[WithBlog]("Compose", "Compose", WithBlog.hasContent _, _.toURLParam)
  }

  object UserPage {
    val menu = Menu.param[User]("User", "User", s => User.findByUsername(s), user => user.username.get)
    val template = Template(() => Templates("user" :: "profile" :: Nil) openOr NodeSeq.Empty)
  }

  object SelectAlbumService {
    val template = Template(() => Templates("log" :: "selectAlbumService" :: Nil) openOr NodeSeq.Empty)
    val menu = Menu.params[WithBlog]("SelectAlbum", "SelectAlbum", WithBlog.apply _, _.toURLParam)
  }

  object SelectAlbum {
    val template = Template(() => Templates("log" :: "selectAlbum" :: Nil) openOr NodeSeq.Empty)
    val menu = Menu.params[WithBlog]("SelectAlbumList", "SelectAlbumList", WithBlog.apply _, _.toURLParam)
  }

  object ViewChapater {
    val template = Template(() => Templates("log" :: "chapater" :: Nil) openOr NodeSeq.Empty)
    val menu = Menu.params[ChapaterInPage]("Chapater", "Chapater", ChapaterInPage.apply _, _.toURLParam)
  }

  object ViewPage {
    val template = Template(() => Templates("log" :: "page" :: Nil) openOr NodeSeq.Empty)
    val menu = Menu.params[PageInfo]("Page", "Page", PageInfo.apply _, _.toURLParam)
  }

}

