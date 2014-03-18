package code.snippet

import code.model._
import code.lib.URLParam._
import code.lib.Implicits._
import code.lib.Session._
import code.lib.UserCheck

import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.StatefulSnippet
import net.liftweb.common.Box
import net.liftweb.common.Empty
import net.liftweb.common.Failure
import net.liftweb.common.Full
import net.liftweb.common.Box._
import scala.util.Try
import net.liftmodules.combobox.ComboBox
import net.liftmodules.combobox.ComboItem
import net.liftweb.http.js.JsExp
import net.liftweb.http.js.JsExp._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmd


class EditBlog(param: WithBlog) extends StatefulSnippet 
{
  private val user = param.user
  private val blog = param.blog

  private var blogTitle: Box[String] = Full(blog.title.get)
  private var description: Box[String] = blog.description.get
  private var tags: List[String] = inTransaction { blog.tags.map(_.tag.get).toList }

  def dispatch = {
    case "render" => render
    case "title" => title
  }

  def title = {
    "title *+" #> s" - 編輯．${blog.title}"
  }

  
  def filterEmptyString(str: String): Box[String] = if (str.trim.isEmpty) Empty else Full(str)

  def searchTag(keyword: String): List[ComboItem] = {
    val tags = for {
      user <- User.currentUser.toList
      tagList <- user.tags.toList
      tag <- tagList
    } yield tag

    tags.distinct.filter(_.contains(keyword)).map(t => ComboItem(t, t))
  }

  def updateTags(tags: List[ComboItem]): JsCmd = { this.tags = tags.map(_.text) }
  val comboBoxOptions: List[(String, JsExp)] = List(
    "multiple" -> JsTrue,
    "width" -> "100%"
  )

  def tagsComboBox = {
    val defaults = tags.map(tag => ComboItem(tag, tag))
    ComboBox(defaults, searchTag _, updateTags _, true, comboBoxOptions)
  }

  def process(value: String) = {
    
    val redirectURL = for {
      user <- User.currentUser ?~ "請先登入"
      title <- this.blogTitle ?~ "請輸入遊記標題"
      blog <- blog.updateBlog(title, description, tags) ?~ "新增遊記失敗，請連絡管理員"
    } yield { s"/user/${user.username}" }

    redirectURL match {
      case Full(url) => S.redirectTo(url)
      case Failure(message, _, _) => S.error(message)
      case Empty => S.error("系統錯誤，請連絡管理員")
    }
  }

  def render = {

    UserCheck.redirectIfNotSameUser(user)

    "#title" #> SHtml.text(blogTitle.openOr(""), value => blogTitle = filterEmptyString(value)) &
    "#description" #> SHtml.textarea(description.openOr(""), value => description = filterEmptyString(value)) &
    "#tags" #> tagsComboBox.comboBox &
    "type=submit" #> SHtml.onSubmit(process _)
  }
}
