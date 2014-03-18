package code.snippet

import code.model._
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


class AddBlog(user: User) extends StatefulSnippet 
{
  private var title: Box[String] = Empty
  private var description: Box[String] = Empty
  private var tags: List[String] = Nil

  def dispatch = {
    case "render" => render
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
  def tagsComboBox = ComboBox(Nil, searchTag _, updateTags _, true, comboBoxOptions)

  def process(value: String) = {
    
    val redirectURL = for {
      user <- User.currentUser ?~ "請先登入"
      title <- this.title ?~ "請輸入遊記標題"
      blog <- Blog.add(title, description, tags, user).toBox ?~ "新增遊記失敗，請連絡管理員"
    } yield { s"/user/${user.username}/log/${blog.id}/selectAlbum" }

    redirectURL match {
      case Full(url) => S.redirectTo(url)
      case Failure(message, _, _) => S.error(message)
      case Empty => S.error("系統錯誤，請連絡管理員")
    }
  }

  def render = {

    UserCheck.redirectIfNotSameUser(user)

    "#title" #> SHtml.text(title.openOr(""), value => title = filterEmptyString(value)) &
    "#description" #> SHtml.textarea(description.openOr(""), value => description = filterEmptyString(value)) &
    "#tags" #> tagsComboBox.comboBox &
    "type=submit" #> SHtml.onSubmit(process _)
  }
}
