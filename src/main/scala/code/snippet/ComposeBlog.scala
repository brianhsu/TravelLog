package code.snippet

import code.lib.URLParam._
import code.lib.UserCheck

import code.model.Photo

import net.liftweb.common.Box

import net.liftweb.http.S
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.SHtml
import net.liftweb.http.StatefulSnippet

import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Helpers._
import net.liftweb.util.ClearClearable
import net.liftweb.util.PassThru
import net.liftweb.common.Full
import net.liftweb.common.Box

import scala.xml.Text
import scala.util.Try
import java.util.Calendar

class ComposeBlog(param: WithBlog) extends StatefulSnippet
{
  private val blog = param.blog
  private val user = param.user

  private var photos = blog.orderedPhotos.getOrElse(Nil).toVector
  private var currentIndex = photos.map(_.status.get).indexOf(Photo.Status.Pending).max(0)

  private def isLast = (currentIndex == photos.size - 1)
  private lazy val defaultChapater = photos.map(_.chapater.get)
  private def currentPhoto = photos(currentIndex)
  private def currentChapater = {
    
    val prevDefaultChapater = defaultChapater(currentIndex-1 max 0)
    val currDefaultChapater = defaultChapater(currentIndex)

    val shouldKeepChapater = 
      (currentIndex > 0) &&
      (currentPhoto.status.get == Photo.Status.Pending) &&
      (prevDefaultChapater == currDefaultChapater)

    if (shouldKeepChapater) photos(currentIndex-1).chapater.get else currentPhoto.chapater.get
  }

  private def stepBar: Vector[(String, Option[String])] = {
    val allChapaters = photos.map(_.chapater.get)
    val originalChapater = currentChapater
    val chapaters = allChapaters.distinct
    val startIndex = chapaters.indexOf(originalChapater) match {
      case n if n <= 0 => 0
      case n if n == chapaters.size-1 => n - 2
      case n => n - 1
    }
    val endIndex = startIndex + 3

    chapaters.slice(startIndex, endIndex).map { chapater => 
      (chapater, if (chapater == originalChapater) Some("active") else None) 
    }
  }

  def dispatch = {
    case "render" => render
    case "photoList" => photoList
    case "title" => title
  }

  def title = {
    "title *+" #> s" - 撰寫．${blog.title}"
  }

  def doAction(action: Photo => Any) {
    action(currentPhoto) match {
      case Full(photo) => photos.updated(currentIndex, photo)
      case photo: Photo => photos = photos.updated(currentIndex, photo)
    }
  }

  def toogle(isDisabled: Boolean)(s: String) = {
    val newStatus = if (isDisabled) Photo.Status.Pending else Photo.Status.Hide
    savePhoto(newStatus)

    if (newStatus == Photo.Status.Hide) {
      currentIndex += 1
    }
  }

  def savePhoto(status: Photo.Status.Status = Photo.Status.Done) {
    doAction { currentPhoto =>
      currentPhoto.editAt(Calendar.getInstance)
      currentPhoto.status(status)
      currentPhoto.saveTheRecord()
    }
  }

  def prevPhoto(s: String) {

    savePhoto(currentPhoto.status.get)

    if (currentIndex > 0) {
      currentIndex -= 1
    }
  }

  def saveAndNext(s: String) {

    val newStatus = currentPhoto.status.get match {
      case Photo.Status.Pending => Photo.Status.Done
      case status => status
    }

    savePhoto(newStatus)

    if (isLast) {
      S.redirectTo(s"/user/${user.username.get}/log/${blog.id}")
    } else {
      currentIndex += 1
    }

  }

  def toValidGPS(points: String): Option[String] = Try { 
    points.split("\\s+").map(_.toDouble)
    points 
  }.toOption

  def isDisable(predicate: => Boolean) = if (predicate) Some("disabled") else None

  def showStepBar = stepBar.map { case(chapater, active) =>
    ".day *" #> chapater &
    ".day [class+]" #> active
  }

  def setNextButtonState = isLast match {
    case false => ClearClearable
    case true =>  
      "#nextButtonTitle *" #> "完成" andThen
      "#nextButtonIcon [class!]" #> "arrow" &
      "#nextButtonIcon [class+]" #> "checkmark" &
      "#nextButton [class!]" #> "black" &
      "#nextButton [class+]" #> "orange"
  }

  def updatePhotoTitle(newTitle: String) = doAction { 
    _.title(Option(newTitle).filterNot(_.trim.isEmpty))
  }

  def toggleHide(index: Int) = {
    
    val newStatus = photos(index).status.get match {
      case Photo.Status.Hide => Photo.Status.Done
      case status => Photo.Status.Hide
    }

    val newPhoto = photos(index).status(newStatus).saveTheRecord()
    newPhoto.foreach(photo => photos = photos.updated(index, photo))

    if (index == currentIndex) {
      S.redirectTo(s"./compose?n=${index}")
    }

    if (newStatus == Photo.Status.Hide) {
      Run(raw""" $$("#photoContent-${index}").addClass("disabled"); """)
    } else {
      Run(raw""" $$("#photoContent-${index}").removeClass("disabled"); """)
    }
  }

  def sideBarPhotosWithIndex = {

    val startIndex = (currentIndex-5).max(0)
    val endIndex = (currentIndex+5).min(photos.size)

    photos.view.zipWithIndex.filter { case(photo, index) =>
      index >= startIndex && index <= endIndex
    }
    
  }

  def photoList = {

    ClearClearable andThen
    "#tocTitle" #> blog.title.get &
    ".photoItem" #> sideBarPhotosWithIndex.map { case(photo, i) =>
      ".photoContent [id]" #> s"photoContent-${i}" &
      ".photoContent [src]" #> photo.thumbnailOrFullImage &
      ".photoContent [class+]" #> isDisable(photo.status.get == Photo.Status.Hide) &
      ".toggleHide [onclick]" #> SHtml.onEvent(s => toggleHide(i)) &
      "a [href]" #> s"./compose?n=${i}"
    }
  }

  def render = {
    
    UserCheck.redirectIfNotSameUser(user)

    val indexParam: Box[Int] = for {
      n <- S.param("n")
      index <- asInt(n)
    } yield index

    currentIndex = indexParam.getOrElse(currentIndex)

    val disableImage = isDisable(currentPhoto.status.get == Photo.Status.Hide)
    val disablePrev = isDisable(currentIndex == 0)

    val photoTitle = currentPhoto.title.get.getOrElse("尚無標題")

    if (photos.isEmpty) {
      S.redirectTo(s"/log/${blog.id}/selectAlbumService", () => S.notice("無法取得相片列表，請再試一次"))
    }

    val editTitleAjax = {
      def title = currentPhoto.title.get
      SHtml.ajaxEditable(
        Text(title.getOrElse("尚無標題")),
        SHtml.text(title.getOrElse(""), newTitle => updatePhotoTitle(newTitle)),
        () => {}
      )
    }

    ClearClearable andThen
    setNextButtonState andThen
    ".day" #> showStepBar &
    "#thumbnailTarget [value]" #> s"photoContent-${currentIndex}"&
    "#preload [src]" #> photos(currentIndex+1 min photos.size-1).thumbnailOrFullImage &
    "#blogTitle *" #> blog.title &
    "#photoTitle *" #> editTitleAjax &
    "#fullImage [href]" #> currentPhoto.photoURL &
    "#fullImage [title]" #> photoTitle &
    "#photoImage [src]" #> currentPhoto.thumbnailOrFullImage &
    "#photoImage [class+]" #> disableImage &
    "#chapater [value]" #> currentChapater &
    "#chapater [disabled]" #> disableImage &
    "#chapater" #> SHtml.onSubmit(value => doAction(_.chapater(value))) &
    "#content" #> SHtml.onSubmit(value => doAction(_.content(Option(value.trim).filterNot(_.isEmpty)))) &
    "#content *" #> currentPhoto.content.get &
    "#content [disabled]" #> disableImage &
    "#gps" #> SHtml.onSubmit(value => doAction(_.gpsPoint(toValidGPS(value)))) &
    "#gps [value]" #> currentPhoto.gpsPoint.get &
    "#gps [disabled]" #> disableImage &
    "#prevButton [disabled]" #> disablePrev &
    "#prevButton [class+]" #> disablePrev &
    "#prevButton" #> SHtml.onSubmit(prevPhoto _) &
    "#deleteButton *" #> disableImage.map(x => "回復").getOrElse("隱藏") &
    "#deleteButton" #> SHtml.onSubmit(toogle(disableImage.isDefined)) &
    "#nextButton" #> SHtml.onSubmit(saveAndNext _) &
    "#currentPhoto" #> (currentIndex + 1) &
    "#totalPhoto" #> photos.size &
    "#epicFile [value]" #> currentPhoto.shortID
  }
}
