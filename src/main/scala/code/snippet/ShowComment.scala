package code.snippet

import code.model._
import code.lib.Implicits._
import code.lib.FadeOutAndRemove

import net.liftweb.util.Helpers._
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._
import net.liftweb.squerylrecord.RecordTypeMode._

import scala.xml.NodeSeq
import scala.xml.Text
import java.text.SimpleDateFormat

object ShowComment
{
  def updateComments(shortID: String) = {
    val snippetName = s"ShowComment?eager_eval=true&shortID=$shortID"

    JqSetHtml("commentDialogHolder", <div data-lift={snippetName}><div data-lift="lift:embed?what=comments" /></div>)
  }

  def showComments(shortID: String)(value: String) = {
    
    Run(s"$$('#commentDialog-$shortID').remove();") &
    updateComments(shortID) &
    Run(s"showDialog('$shortID')")
  }

}

class ShowComment
{
  import net.liftweb.util.PassThru
  import net.liftweb.common.{Box, Empty}

  private var message: String = _

  def getCommentInfo = inTransaction {
    val photo = for {
      shortID <- S.attr("shortID")
      photo <- Photo.findByShortID(shortID)
    } yield photo

    val comments = photo.map(p => from(p.comments) { c => select(c) orderBy c.dateTime.desc}.toList) getOrElse Nil
    val users = for {
      userID <- comments.map(_.userID.get).distinct
      user <- User.findByID(userID)
    } yield (userID, user)

    (photo, comments, users.toMap)
  }

 
  def addMessage(shortIDAttr: Box[String])(value: String) = {

    val comment = inTransaction {
      for {
        shortID <- shortIDAttr
        photo <- Photo.findByShortID(shortID)
        commentUser <- User.currentUser
        message <- (Box !! message).filterNot(_.trim.size == 0)
        comment <- Comment.createRecord.photoID(photo.id).userID(commentUser.id).content(message).saveTheRecord()
      } yield (photo, comment, photo.comments.size)
    }

    def updateCommentDialog(commentInfo: (Photo, Comment, Int)) = {
      val (photo, comment, count) = commentInfo
      val shortID = shortIDAttr.getOrElse("")

      SetHtml(s"count-${shortID}", Text(count.toString)) &
      PrependHtml(s"commentList-${shortID}", createCommentRow(photo, comment, User.currentUser))
    }

    comment.isDefined match {
      case true  => updateCommentDialog(comment.get)
      case false => Noop
    }
  }

  def messageForm = {
    
    "textarea" #> SHtml.ajaxTextarea("", message = _) &
    ".okButton [onclick]" #> SHtml.onEvent(addMessage(S.attr("shortID")))

  }

  def createCommentRow(photo: Photo, comment: Comment, user: Box[User]) = {

    import net.liftweb.http.Templates
    
    val template = Templates("templates-hidden" :: "commentRow" :: Nil)

    def deleteComment(s: String) = {

      comment.delete()

      val count = inTransaction { photo.comments.size }
      val shortID = photo.shortID

      SetHtml(s"count-${shortID}", Text(count.toString)) &
      FadeOutAndRemove(s"commentRow-${comment.id}")
    }

    val cssBinding = {
      val dateFormatter = new SimpleDateFormat("yyyy-MM-dd HH:mm")
      val commenterID = user.map(_.id).getOrElse(-1)
      val username = user.map(_.username.get).getOrElse("")
      val nickname = user.map(_.nickname.get).getOrElse("")
      val avatar = user.map(_.avatarURL).getOrElse("http://www.gravatar.com/avatar/none?d=mm")
      val isAuthorOrCommenter = {
        User.currentUser.
             map(currentUser => currentUser.id == commenterID || currentUser.id == photo.blogAuthor.get.id).
             getOrElse(false)
      }

      ".commentRow [id]" #> s"commentRow-${comment.id}" &
      ".commentUserLink [href]" #> s"/user/${username}" &
      ".commentUserAvatar [src]" #> avatar &
      ".commentUserNickname *" #> nickname &
      ".commentContent *" #> comment.content.get.markdownToHTML.getOrElse(Text(comment.content.get)) &
      ".dateTime *" #> dateFormatter.format(comment.dateTime.get.getTime) &
      ".action" #> (if (isAuthorOrCommenter) Some(PassThru) else None) andThen
      ".action" #> (
        ".deleteButton [onclick]" #> SHtml.onEventIf("確定要刪除嗎？這個動作無法還原喲！", deleteComment _)
      )
    }

    template.map(cssBinding).getOrElse(<div>Could not render comment row</div>)

  }

  def render = {
    
    val shortID = S.attr("shortID").getOrElse("")
    val (photo, comments, users) = getCommentInfo
    val hideSystemHint = if (comments.isEmpty) Some(PassThru) else None

    ".commentDialog [id]" #> s"commentDialog-${shortID}" &
    ".commentList [id]" #> s"commentList-${shortID}" &
    ".messageForm" #> (if (User.isLoggedIn) Some(messageForm) else None) &
    ".loginHint" #> (if (User.isLoggedIn) None else Some(PassThru)) &
    ".thumbImage [src]" #> photo.map(_.thumbnailOrFullImage) &
    ".commentSystem" #> hideSystemHint &
    ".commentRow" #> comments.map(comment => createCommentRow(photo.get, comment, users.get(comment.userID.get)))
  }
}

