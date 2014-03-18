package code.snippet

import code.lib._
import code.model._
import code.lib.Session._
import code.lib.Implicits._

import net.liftweb.util.Helpers._
import net.liftweb.util.ClearClearable
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsCmd
import net.liftweb.squerylrecord.RecordTypeMode._

import scala.xml.NodeSeq
import scala.xml.Text
import scala.util.Try
import java.text.SimpleDateFormat

class UserProfile(user: User) {

  val blogs = inTransaction { from(user.blogs) { blog => select(blog).orderBy(blog.id.desc) }.toList }
  val dateFormatter = new SimpleDateFormat("yyyy-MM-dd HH:mm")

  def hideAddBlogButton = "#addBlogButton" #> NodeSeq.Empty
  def showTitle = {
    ".avatar [src]" #> user.avatarURL &
    ".blogTitle *" #> user.logtitle.get
  }

  def getBlogDescription(blog: Blog): Option[NodeSeq] = {
    for {
      desc <- blog.description.get
      html <- desc.markdownToHTML.toOption
    } yield html
  }

  def deleteBlog(blog: Blog): JsCmd = {
    blog.delete()
    FadeOutAndRemove(s"item-${blog.id}")
  }

  def getBlogStatus(blog: Blog) = {

    val isEmpty = blog.isEmpty.getOrElse(true) 
    val notModified = blog.hasNoModified.getOrElse(true)
    val isDone = blog.isDone.getOrElse(false)

    blog match {
      case _ if isEmpty || notModified => ("還沒寫", "")
      case _ if isDone => ("已寫完", "teal")
      case _ => ("正在寫", "purple")
    }
  }

  def tagList(blog: Blog) = {
    Try(inTransaction(blog.tags.toList)).getOrElse(Nil).map { blogTag =>
      ".tags *" #> blogTag.tag.get
    }
  }

  def item(blog: Blog) =  {
    
    val isEmpty = blog.isEmpty.getOrElse(true) 
    val notModified = blog.hasNoModified.getOrElse(true)
    val isDone = blog.isDone.getOrElse(false)

    val disableView = if (isEmpty || notModified) Some("disabled") else None
    val viewURL = if (isEmpty || notModified) "#" else s"/user/${user.username}/log/${blog.id}"
    val editURL = if (isEmpty) s"/user/${user.username}/log/${blog.id}/selectAlbum" else s"/user/${user.username}/log/${blog.id}/compose" 

    val (statusText, statusColor) = getBlogStatus(blog)

    ".image [src]" #> blog.coverURL.getOrElse("/assets/images/dummy.jpg") &
    ".item [id]" #> s"item-${blog.id}" &
    ".title *" #> blog.title &
    ".description *" #> getBlogDescription(blog) &
    ".view [href]" #> viewURL &
    ".view [class+]" #> disableView &
    ".editLink [href]" #> editURL &
    ".editLink *" #> (if (isEmpty) "選擇相簿" else "撰寫") &
    ".delete [onclick]" #> SHtml.onEventIf(s"確定要刪除 ${blog.title} 嗎？這個動作無法還原喲！", s => deleteBlog(blog)) &
    ".status [class+]" #> statusColor &
    ".status *" #> statusText &
    ".refreshLink [href]" #> s"/user/${user.username}/log/${blog.id}/selectAlbum" &
    ".editBlogLink [href]" #> s"/user/${user.username}/log/${blog.id}/edit" &
    ".tags" #> tagList(blog) &
    ".lastUpdated" #> blog.lastUpdated.map(calendar => dateFormatter.format(calendar.getTime))
  }

  def viewOnlyItem(blog: Blog) =  {

    val (statusText, statusColor) = getBlogStatus(blog)

    ".image [src]" #> blog.coverURL.getOrElse("/assets/images/dummy.jpg") &
    ".item [id]" #> s"item-${blog.id}" &
    ".title *" #> blog.title &
    ".description *" #> getBlogDescription(blog) &
    ".view [href]" #> s"/user/${user.username}/log/${blog.id}" &
    ".editLink" #> NodeSeq.Empty &
    ".delete" #> NodeSeq.Empty &
    ".status [class+]" #> statusColor & 
    ".status *" #> statusText &
    ".refreshLink" #> NodeSeq.Empty &
    ".editBlogLink" #> NodeSeq.Empty &
    ".tags" #> tagList(blog) &
    ".lastUpdated" #> blog.lastUpdated.map(calendar => dateFormatter.format(calendar.getTime))

  }

  def hasNoContent(blog: Blog) = blog.isEmpty.getOrElse(true) || blog.hasNoModified.getOrElse(true)

  def title = {
    "title *+" #> s" - ${user.logtitle}"
  }

  def render = {
    user.isCurrentUser match {
      case true  => 
        ClearClearable andThen 
        showTitle & 
        "#addBlogButton [href]" #> s"/user/${user.username}/log/new" &
         ".item" #> blogs.map(item)
      case false => 
        ClearClearable andThen 
        showTitle & 
        hideAddBlogButton & 
        ".item" #> blogs.filterNot(hasNoContent _).map(viewOnlyItem)
    }
  }
}
