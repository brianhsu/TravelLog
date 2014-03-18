package bootstrap.liftweb

import code.model._
import code.lib.Session._
import code.lib.Implicits._

import net.liftweb.http.S
import net.liftweb.http.LiftRules
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.Req
import net.liftweb.http.XHtmlInHtml5OutProperties
import net.liftweb.http.NotFoundAsTemplate
import net.liftweb.http.ParsePath
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.NotFoundResponse
import net.liftweb.http.InMemoryResponse
import net.liftweb.util.Helpers._

import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc.If
import net.liftweb.sitemap.Loc.Unless

import net.liftweb.util.NamedPF
import net.liftweb.common.Full
import net.liftmodules.combobox.ComboBox
import net.liftweb.squerylrecord.RecordTypeMode._

import javax.imageio.ImageIO

import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.Format
import scala.util.Try
import java.net.URL
import java.io.ByteArrayOutputStream
import net.liftweb.common.Box

object ImageService extends RestHelper {


  def coverImage(shortID: String, width: String, height: String) = {

    def cachedImage: Box[Array[Byte]] = for {
      width <- asInt(width)
      height <- asInt(height)
      photo <- Photo.findByShortID(shortID)
      imageCache <- ImageCache.findByPhotoID(photo.id, width, height)
    } yield imageCache.image.get

    def coverImage: Box[Array[Byte]] = for {
      width <- asInt(width)
      height <- asInt(height)
      photo <- Photo.findByShortID(shortID)
      bufferedImage <- Try(ImageIO.read(new URL(photo.thumbnailOrFullImage))).toBox
    } yield {
      val image = Image(bufferedImage).cover(width, height).write(Format.JPEG)
      inTransaction { 
        ImageCache.createRecord.photoID(photo.id).
                   width(width).height(height).
                   image(image).saveTheRecord()
      }
      image
    }

    cachedImage or coverImage match {
      case Full(image) => InMemoryResponse(image, "Content-Type" -> "image/jpeg" :: Nil, Nil, 200)
      case _ => NotFoundResponse()
    }

  }

  serve {
    case "images" :: "cover" :: shortID :: width :: height :: Nil Get req => coverImage(shortID, width, height)
  }

  def init() : Unit = {
    LiftRules.statelessDispatch.append(ImageService)
  }

}

class Boot 
{
  import ParamMenu._

  def needLogin = If(() => User.isLoggedIn, () => S.redirectTo("/", () => S.notice("請先登入")))

  lazy val siteMap = SiteMap(
    Menu("Home") / "index",
    Menu("About") / "about",
    Menu("Login") / "login" / "index" >> Unless(() => User.isLoggedIn, () => S.redirectTo(s"/user/${User.currentUser.get.username.get}")),
    Menu("SignUp") / "login" / "signup" >> If(() => FirstLoginEmail.is.isDefined, () => S.redirectTo("/")),
    Menu("LoginWithGoogle") / "login" / "google" >> GoogleLogin.redirectToAuthPage,
    Menu("LoginWithImgUr") / "login" / "imgUr" >> ImgUrLogin.redirectToAuthPage,
    Menu("AuthWithGoogle") / "auth" / "google" >> GoogleLogin.checkAuthorization,
    Menu("AuthWithImgUr") / "auth" / "imgUr" >> ImgUrLogin.checkAuthorization,
    Menu("AlbumWithPicasa") / "log" / "selectAlbum" / "googleAuth" >> PicasaWebAuth.checkAuthorization,
    Menu("AlbumWithFlickr") / "log" / "selectAlbum" / "flickrAuth" >> FlickrAuth.checkAuthorization,
    Menu("AlbumWithImgUr") / "log" / "selectAlbum" / "imgUrAuth" >> ImgUrAuth.checkAuthorization,
    UserPage.menu / "user" / * >> UserPage.template,
    AddBlog.menu / "user" / * / "log" / "new" >> AddBlog.template >> needLogin,
    EditBlog.menu / "user" / * / "log" / * / "edit" >> EditBlog.template,
    ComposeBlog.menu / "user" / * / "log" / * / "compose" >> ComposeBlog.template >> needLogin,
    SelectAlbumService.menu / "user" / * / "log" / * / "selectAlbum" >> SelectAlbumService.template >> needLogin,
    SelectAlbum.menu / "user" / * / "log" / * / "selectAlbum" / "select" >> SelectAlbum.template >> needLogin,
    ViewChapater.menu / "user" / * / "log" / * / "chapater" / * >> ViewChapater.template,
    ViewPage.menu / "user" / * / "log" / * >> ViewPage.template
  )

  def boot 
  {
    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))
    LiftRules.addToPackages("code")
    LiftRules.setSiteMap(siteMap)

    LiftRules.ajaxStart = Full(() => Run("""$('#ajaxLoader').dimmer({closable: false}).dimmer('show'); $('#ajaxSpinner').fadeIn('fast')"""))
    LiftRules.ajaxEnd = Full(() => Run("""$('#ajaxLoader').dimmer({closable: false}).dimmer('hide'); $('#ajaxSpinner').fadeOut('fast')"""))

    LiftRules.htmlProperties.default.set { r: Req => new XHtmlInHtml5OutProperties(r.userAgent) }
    LiftRules.uriNotFound.prepend(NamedPF("404handler"){
      case (req,failure) => NotFoundAsTemplate(ParsePath(List("404"),"html",true,false))
    })

    DBSetting.initDB()
    ImageService.init()
    ComboBox.init
  }
}

