package code.snippet

import code.lib.URLParam._
import code.lib.Implicits._
import code.lib.Session._
import code.model._

import net.liftweb.util.Helpers._
import net.liftweb.util.ClearClearable
import net.liftweb.util.Props
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmds._
import net.liftweb.squerylrecord.RecordTypeMode._

import scala.xml.NodeSeq
import scala.xml.Text

class Chapater(chapaterInPage: ChapaterInPage)
{
  val page = chapaterInPage.page
  val blog = page.blog
  val username = chapaterInPage.username
  val chapater = chapaterInPage.chapater
  val hostAndPath = Props.get("CALLBACK_HOST", S.hostAndPath)
  val photoIDToSN = blog.orderedPhotos.getOrElse(Nil).map(_.id).zipWithIndex.toMap
  val isCurrentUser = CurrentUser.get.map(_.username.get == username).getOrElse(false)

  def head = {
    val title = s" - ${blog.title} ． ${chapater.title}"
    "title *+" #> title &
    "rel=canonical [href]" #> s"$hostAndPath/user/$username/log/${blog.id}/chapater/${chapater.currentChapater}" &
    "name=title [content]" #> ("TravelLog" + title) &
    "property=og:image [content]" #> chapater.allPhotos.headOption.map(_.thumbnailOrFullImage) &
    "name=description [content]" #> chapater.mainPhotos.headOption.flatMap(_.content.get)
  }

  def mainMenu = {
    
    val currentChapater = chapater.currentChapater
    val disablePrev = if (currentChapater == 0) Some("disabled") else None
    val disableNext = if (currentChapater+1 == chapater.maxChapater) Some("disabled") else None
    val prevLink = if (disablePrev.isEmpty) Some(s"/user/${username}/log/${blog.id}/chapater/${currentChapater-1}") else None
    val nextLink = if (disableNext.isEmpty) Some(s"/user/${username}/log/${blog.id}/chapater/${currentChapater+1}") else None

    "#blogTitle *" #> page.title &
    "#chapaterName *" #> chapater.title &
    "#pageNumber *" #> s"${currentChapater + 1} / ${chapater.maxChapater}" &
    "#authorNickname *" #> page.authorNickname &
    "#authorLink [href]" #> s"/user/${page.authorUsername}" &
    "#prevButton [href]" #> prevLink &
    "#prevButton [class+]" #> disablePrev &
    "#nextButton [href]" #> nextLink &
    "#nextButton [class+]" #> disableNext
  }

  def tableOfContent = {
    ClearClearable &
    "#tocTitle" #> page.title &
    ".chapater" #> page.chapaters.zipWithIndex.map { case(title, n) =>
      
      val isActive = if (n == chapater.currentChapater) Some("active") else None

      ".chapater *" #> title &
      ".chapater [href]" #> s"/user/${username}/log/${blog.id}/chapater/${n}" &
      ".chapater [class+]" #> isActive
    }
  }

  def thumbnailList = {
    ClearClearable andThen
    "li" #> chapater.allPhotos.map { photo =>
      ".thumbLink [href]" #> photo.photoURL &
      ".thumbImage [src]" #> photo.thumbnailOrFullImage &
      ".thumbLink [title]" #> photo.title.get
    }
  }

  def renderPhoto(photo: Photo) = {

    val commentCounts = inTransaction(photo.comments.size)
    val htmlContent  = {
      for {
        content <- photo.content.get
        html <- content.markdownToHTML.toOption
      } yield html
    }

    val photoSN = photoIDToSN.get(photo.id).getOrElse(0)
    val editURL = s"$hostAndPath/user/$username/log/${blog.id}/compose?n=${photoSN}"

    ".photoTitle *" #> photo.title.get.getOrElse("尚無標題") &
    ".imageLink [href]" #> photo.photoURL &
    ".photoImage [src]" #> photo.thumbnailOrFullImage &
    ".article *" #> htmlContent.getOrElse(Text(photo.content.get.getOrElse(""))) &
    ".count *" #> commentCounts &
    ".count [id]" #> s"count-${photo.shortID}" &
    ".commentLabel [onclick]" #> SHtml.onEvent(ShowComment.showComments(photo.shortID)) &
    ".gpsLabel" #> photo.gpsPoint.get.map { point =>
      "a [href]" #> s"https://maps.google.com/maps?q=${point.replace(" ", ",")}&z=17"
    } &
    ".editLabel" #> (if (isCurrentUser) ("a [href]" #> editURL) else ".editLabel" #> NodeSeq.Empty)
  }

  def renderArticle = {

    val groupedPhotos = chapater.mainPhotos.grouped(2).toList

    ".contents" #> groupedPhotos.map { photos =>

      if (photos.size < 2) {
        ".odd" #> renderPhoto(photos(0)) &
        ".even" #> NodeSeq.Empty
      } else {
        ".odd" #> renderPhoto(photos(0)) &
        ".even" #> renderPhoto(photos(1))
      }

    }
  }

  def render = {
    
    if (chapater.mainPhotos.isEmpty) {
      ".noArticle ^^" #> NodeSeq.Empty &
      ".photoL" #> thumbnailList
    } else {
      ".noArticle" #> NodeSeq.Empty andThen
      renderArticle
    }

  }

}
