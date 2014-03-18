package code.snippet

import bootstrap.liftweb._

import code.comet._
import code.model._
import code.lib._
import code.lib.Session._
import code.lib.Implicits._
import code.lib.URLParam._
import code.lib.UserCheck

import org.bone.sphotoapi.api.PicasaWebAPI
import org.bone.sphotoapi.api.API
import org.bone.sphotoapi.model.Album
import org.bone.sphotoapi.model.AlbumPrivacy.Public
import org.bone.sphotoapi.model.{Photo => SPhoto}
import org.bone.sphotoapi.model.GPSPoint

import net.liftweb.util.Helpers._
import net.liftweb.util.PassThru

import net.liftweb.http.RequestVar
import net.liftweb.http.SessionVar
import net.liftweb.http.NamedCometListener
import net.liftweb.http.SHtml
import net.liftweb.http.S
import net.liftweb.squerylrecord.RecordTypeMode._

import net.liftweb.common._

import org.joda.time.DateTime
import org.joda.time.Days

import java.util.Calendar
import java.util.Date

object SelectAlbumService {
  object SelectedAlbum extends RequestVar[Box[Album]](Empty)
  object PhotoAPI extends SessionVar[Box[API]](Empty)
}

class SelectAlbumService(param: WithBlog) {

  private val blog = param.blog
  private val user = param.user

  private def notSelected = SelectAlbumService.SelectedAlbum.isEmpty
  private def selectedAlbum = SelectAlbumService.SelectedAlbum openOr createDummyAlbum
  private val isResync: Boolean = (for {
    photos <- param.blog.orderedPhotos
    isAllPending = photos.forall(_.status.get == Photo.Status.Pending)
  } yield !isAllPending).getOrElse(false)

  def title = {
    "title *+" #> s" - 選擇相簿服務．${blog.title}"
  }

  def createDummyAlbum = {
    if (isResync) {
      new Album("", blog.title.get, None, new Date, blog.coverURL.getOrElse(""), Public, "")
    }
    else {
      new Album("", "尚未選擇", None, new Date, "http://i.imgur.com/rojIv6D.jpg", Public, "")
    }
  }

  val cometName = java.util.UUID.randomUUID.toString

  def addNewPhoto(photoAPI: API, photo: SPhoto, firstDay: DateTime)  {
    val photoCreated = new DateTime(photo.timestamp)
    val lastUpdated = new DateTime(photo.lastUpdated)
    val days = Days.daysBetween(firstDay, photoCreated).getDays + 1
    val title = s"第 ${days} 天"
    val idInService = photoAPI.serviceName + "-" + photo.id
    val thumbnailURL = photo.thumbnails.find(_.maxSize >= 480).map(_.url)

    var photoInDB = 
      Photo.createRecord.
          photoURL(photo.imageURL).blogID(blog.id).chapater(title).
          title(photo.title).photoCreatedAt(photoCreated.toCalendar(null)).
          idInService(idInService).thumbnailURL(thumbnailURL).
          lastUpdated(lastUpdated.toCalendar(null))

    photo.location.foreach { case GPSPoint(lat, long) =>
      photoInDB = photoInDB.gpsPoint(s"$lat $long")
    }

    photoInDB.saveTheRecord()
  }

  def updatePhoto(photoInDB: Photo, photo: SPhoto) {

    val photoCreated = new DateTime(photo.timestamp)
    val lastUpdated = new DateTime(photo.lastUpdated)
    val thumbnailURL = photo.thumbnails.find(_.maxSize >= 720).map(_.url)

    var newPhotoInDB = 
      photoInDB.photoURL(photo.imageURL).title(photo.title).
                photoCreatedAt(photoCreated.toCalendar(null)).
                thumbnailURL(thumbnailURL).lastUpdated(lastUpdated.toCalendar(null))

    photo.location.foreach { case GPSPoint(lat, long) =>
      newPhotoInDB = photoInDB.gpsPoint(s"$lat $long")
    }

    newPhotoInDB.saveTheRecord()
  }

  def resyncPhotos(value: String) {
    val dispatcher = NamedCometListener.getDispatchersFor(Full(cometName)).get
    
    for {
      comet <- dispatcher
      photoAPI <- SelectAlbumService.PhotoAPI
      photoList <- photoAPI.getPhotos(selectedAlbum.id).toBox
    } inTransaction {

      if (photoList.size == 0) {
        S.notice("相簿中沒有照片，請重新選擇")
        return;
      }

      val firstDay = new DateTime(photoList.map(_.timestamp.getTime).min)

      photoList.zipWithIndex.foreach { case (photo, i) =>

        val idInService = photoAPI.serviceName + "-" + photo.id
        val oldPhotos = Photo.findByIDInService(idInService)

        oldPhotos match {
          case Full(photoInDB) => updatePhoto(photoInDB, photo)
          case Empty => addNewPhoto(photoAPI, photo, firstDay)
          case Failure(msg, _, _) => S.error("無法新增或同步照片，請稍候再試一次")
        }

        comet ! Progress(((i + 1).toDouble / photoList.size) * 100)
      }

    }

    S.redirectTo(s"/user/${user.username}/log/${blog.id}/compose")

  }

  def savePhotos(value: String) {

    val dispatcher = NamedCometListener.getDispatchersFor(Full(cometName)).get
    
    for {
      comet <- dispatcher
      photoAPI <- SelectAlbumService.PhotoAPI
      photoList <- photoAPI.getPhotos(selectedAlbum.id).toBox
    } inTransaction {

      if (photoList.size == 0) {
        S.notice("相簿中沒有照片，請重新選擇")
        return;
      }

      blog.photos.deleteAll

      val firstDay = new DateTime(photoList.map(_.timestamp.getTime).min)

      photoList.zipWithIndex.foreach { case (photo, i) =>
        addNewPhoto(photoAPI, photo, firstDay)
        comet ! Progress(((i + 1).toDouble / photoList.size) * 100)
      }

    }

    S.redirectTo(s"/user/${user.username}/log/${blog.id}/compose")
  }

  def render = {

    UserCheck.redirectIfNotSameUser(user)

    val disabledOption = if (notSelected) Option("disabled") else None
    val updateOKText = if (isResync) "重新同步" else "完成"

    val onResync = SHtml.onEventIf("這個遊記裡已經有內容了，確定要重新同步或加入新的相簿嗎？", resyncPhotos _)
    val onUpload = SHtml.onEvent(savePhotos _)

    "#selectGoogle [onclick]" #> SHtml.onEvent(s => PicasaWebAuth.redirectToAuthPage(blog.id)) &
    "#selectFlickr [onclick]" #> SHtml.onEvent(s => FlickrAuth.redirectToAuthPage(blog.id)) &
    "#selectImgUr [onclick]" #> SHtml.onEvent(s => ImgUrAuth.redirectToAuthPage(blog.id)) &
    "#albumTitle *" #> selectedAlbum.title &
    "#albumImage [src]" #> selectedAlbum.coverURL &
    "#okButton *" #> updateOKText &
    "#okButton [class+]" #> disabledOption &
    "#okButton [disabled+]" #> disabledOption &
    "#okButton [onclick]" #> (if (isResync) onResync else onUpload) &
    "#comet" #> <div data-lift={s"comet?type=ProgressBar;name=${cometName}"}></div>
  }
}
