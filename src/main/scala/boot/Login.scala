package bootstrap.liftweb

import org.bone.sphotoapi.api._

import net.liftweb.common.Full
import net.liftweb.http.RedirectResponse
import net.liftweb.http.S
import net.liftweb.http.SessionVar
import net.liftweb.sitemap.Loc.EarlyResponse
import net.liftweb.util.StringHelpers
import scala.util.{Success, Failure}
import net.liftweb.util.Helpers.tryo
import net.liftweb.util.Helpers.asLong

import net.liftweb.squerylrecord.RecordTypeMode._
import code.model._
import net.liftweb.common.{Failure => LFailure, _}
import code.lib.Session._
import code.lib.Implicits._
import code.snippet._
import net.liftweb.util.Props

object CallbackURL
{
  lazy val hostAndPath = Props.get("CALLBACK_HOST", S.hostAndPath)
  def apply(path: String) = s"${hostAndPath}${path}"
}


object FlickrAuth
{
  private val apiKey = Props.get("FLICKR_APIKEY").getOrElse("InvalidKey")
  private val apiSecret = Props.get("FLICKR_SECRET").getOrElse("InvalidSecret")
 
  private lazy val callbackURL = CallbackURL("/log/selectAlbum/flickrAuth")

  object BlogID extends SessionVar[Box[Long]](Empty)

  def redirectToAuthPage(blogID: Long) {

    val api = FlickrAPI.withCallback(apiKey, apiSecret, callbackURL)

    SelectAlbumService.PhotoAPI(Full(api))

    api.getAuthorizationURL("perms" -> "read") match {
      case Failure(error) => S.error("無法轉至登入頁面，請稍後再試")
      case Success(url) => 
        BlogID(Full(blogID))
        S.redirectTo(url)
    }
  }

  def checkAuthorization = EarlyResponse { () =>

    val api = SelectAlbumService.PhotoAPI.is.openOrThrowException("無法取得 PhotoAPI 物件")
    val redirectURL = for {
      blogID <- BlogID.get ?~ "無法取得 state 參數"
      blog <- Blog.findByID(blogID)
      user <- blog.blogAuthor
      code <- S.param("oauth_verifier") ?~ "無法取得 oauth_verifier 參數"
      isOK <- api.authorize(code).toBox
    } yield { s"/user/${user.username}/log/${blogID}/selectAlbum/select" }
      
    redirectURL match {
      case Full(url) => S.redirectTo(url)
      case _ => S.redirectTo("/", () => S.error("無法登入 Flickr 相簿，請稍候再試"))
    }

  }

}

abstract class DefaultOAuth20Auth {
  protected def getAPI: API
  protected val apiKey: String
  protected val apiSecret: String
  protected val callbackURL: String

  def redirectToAuthPage(blogID: Long) {

    val api = getAPI

    SelectAlbumService.PhotoAPI(Full(api))

    api.getAuthorizationURL("state" -> blogID.toString) match {
      case Success(url) => S.redirectTo(url)
      case Failure(error) => S.error("無法轉至登入頁面，請稍後再試")
    }
  }

  def checkAuthorization = EarlyResponse { () =>

    val api = SelectAlbumService.PhotoAPI.is.openOrThrowException("無法取得 PhotoAPI 物件")
    val redirectURL = for {
      blogID <- asLong(S.param("state")) ?~ "無法取得 state 參數"
      blog <- Blog.findByID(blogID)
      user <- blog.blogAuthor
      code <- S.param("code") ?~ "無法取得 code 參數"
      isOK <- api.authorize(code).toBox
    } yield { s"/user/${user.username}/log/${blogID}/selectAlbum/select" }
      
    redirectURL match {
      case Full(url) => S.redirectTo(url)
      case _ => S.redirectTo("/", () => S.error("無法登入 PicasaWeb，請稍候再試"))
    }

  }

}

object ImgUrAuth extends DefaultOAuth20Auth
{
  override val apiKey = Props.get("IMGUR_APIKEY").getOrElse("InvalidKey")
  override val apiSecret = Props.get("IMGUR_SECRET").getOrElse("InvalidSecret")
  override lazy val callbackURL = CallbackURL("/log/selectAlbum/imgUrAuth")
  override def getAPI = ImgUrAPI.withCallback(apiKey, apiSecret, callbackURL)
}


object PicasaWebAuth extends DefaultOAuth20Auth
{
  override val apiKey = Props.get("PICASAWEB_KEY").getOrElse("InvalidKey")
  override val apiSecret = Props.get("PICASAWEB_SECRET").getOrElse("InvalidSecret")
  override lazy val callbackURL = CallbackURL("/log/selectAlbum/googleAuth")
  override def getAPI = PicasaWebAPI.withCallback(apiKey, apiSecret, callbackURL)
}

abstract class DefaultOAuth20Login
{
  protected def getAPI: API
  protected val apiKey: String
  protected val apiSecret: String
  protected val callbackURL: String
  protected val loginService: User.LoginService.Value

  private object OAuthState extends SessionVar[Option[String]](None)
  private object PhotoAPI extends SessionVar[Option[API]](None)

  def redirectToAuthPage = EarlyResponse { () => 

    val api = getAPI

    OAuthState.set(Some(StringHelpers.randomString(48)))
    PhotoAPI.set(Some(api))

    api.getAuthorizationURL("state" -> OAuthState.is.get) match {
      case Success(url) => Full(RedirectResponse(url))
      case Failure(error) => S.redirectTo("/login/", () => S.error("無法轉至登入頁面，請稍後再試"))
    }
  }

  def login(userInfo: (String, String)) = {

    val (userID, email) = userInfo

    def getUserFromDB = inTransaction { User.findByEmail(email, loginService) }.oToBox

    getUserFromDB match {
      case Full(user) => user.login(); S.redirectTo(s"/user/${user.username.get}")
      case Empty => FirstLoginEmail.set(Some(email)); S.redirectTo("/login/signup?accountType=" + loginService)
      case LFailure(message, _, _) => S.redirectTo("/login", () => S.error(message))
    }
  }

  def checkAuthorization = EarlyResponse { () =>

    def getState = for {state <- S.param("state") ~> "無法取得 state 參數"} yield OAuthState.is.exists(_ == state)
    def getAuthStatus = for {code <- S.param("code") ~> "無法取得 code 參數"} yield PhotoAPI.is.map(_.authorize(code))

    val userInfo = for {
      state <- getState if state == true
      status <- tryo { getAuthStatus.get }
      userInfo <- tryo { PhotoAPI.is.get.getUserInfo.get }
    } yield userInfo

    userInfo match {
      case Full(userInfo) => login(userInfo)
      case _ => S.redirectTo("/login/", () => S.error("無法登入，請重試一次"))
    }
  }
}

object ImgUrLogin extends DefaultOAuth20Login
{
  override val apiKey = Props.get("IMGUR_LOGIN_APIKEY").getOrElse("InvalidKey")
  override val apiSecret = Props.get("IMGUR_LOGIN_SECRET").getOrElse("InvalidSecret")
  override val loginService = User.LoginService.ImgUr
  override lazy val callbackURL = CallbackURL("/auth/imgUr")
  override def getAPI = ImgUrAPI.withCallback(apiKey, apiSecret, callbackURL)
}


object GoogleLogin extends DefaultOAuth20Login
{
  override val apiKey = Props.get("PICASAWEB_KEY").getOrElse("InvalidKey")
  override val apiSecret = Props.get("PICASAWEB_SECRET").getOrElse("InvalidSecret")
  override val loginService = User.LoginService.Google
  override lazy val callbackURL = CallbackURL("/auth/google")
  override def getAPI = PicasaWebAPI.withCallback(apiKey, apiSecret, callbackURL)
}

