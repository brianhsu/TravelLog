package code.snippet

import code.lib.URLParam._
import code.lib.UserCheck

import org.bone.sphotoapi.model.AlbumPrivacy.Public
import org.bone.sphotoapi.model._

import code.lib._
import code.model._
import code.lib.Session._
import code.lib.Implicits._

import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmds._

import net.liftweb.util.Helpers._
import net.liftweb.util.ClearClearable
import net.liftweb.util.PassThru

import net.liftweb.common._
import java.text.SimpleDateFormat
import net.liftweb.http.DispatchSnippet
import scala.xml.Text

class SelectAlbum(param: WithBlog) {

  private val blog = param.blog
  private val user = param.user

  private val dateFormater = new SimpleDateFormat("yyyy-MM-dd")
  private var selectedAlbum: Box[Album] = Empty


  def title = {
    "title *+" #> s" - 選擇相簿．${blog.title}"
  }

  def getGroupedAlbums: Box[List[List[Album]]] = {

    for {
      photoAPI <- SelectAlbumService.PhotoAPI.is ?~ "無法取得 PhotoAPI 以連接 PicasaWeb"
      albums <- photoAPI.getAlbums().toBox ?~ "無法取得相簿列表"
    } yield albums.filter(_.privacy == Public).grouped(5).toList

  }

  def redirectWithErrorMessage(message: String) {
    S.redirectTo(s"/user/${user.username}/log/${blog.id}/selectAlbum", () => S.error(message))
  }

  def showItem(album: Album) = {
    
    def showModal() = {
      this.selectedAlbum = Box !! album

      val coverURL = album.coverURL
      val title = album.title
      val dateTime = dateFormater.format(album.dateTime)
      val description = album.description.getOrElse("")

      Run(raw"""showDialog("$coverURL", "$title", "$dateTime", "$description");""")
    }

    ".name *" #> album.title &
    ".albumImage [src]" #> album.coverURL &
    ".item [onclick]" #> SHtml.onEvent(s => showModal())
  }

  def setSelectedAlbum(s: String) {
    S.redirectTo(
      s"/user/${user.username}/log/${blog.id}/selectAlbum", 
      () => SelectAlbumService.SelectedAlbum(this.selectedAlbum)
    )
  }

  def showItemList(groupedAlbums: List[List[Album]]) = {

    ClearClearable andThen 
    "#confirmButton [onclick]" #> SHtml.onEvent(setSelectedAlbum) &
    ".items" #> groupedAlbums.map { albums => 
      ".item" #> albums.map(showItem) 
    }
  }

  def render = {

    UserCheck.redirectIfNotSameUser(user)

    getGroupedAlbums match {
      case Full(groupedAlbums) => showItemList(groupedAlbums)
      case Empty    => "*" #> Text("無法取得相簿列表")
      case Failure(msg, _, _) => "*" #> Text("無法取得相簿列表：" + msg)
    }
  }

}

