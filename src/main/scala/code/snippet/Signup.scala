package code.snippet

import code.model.User
import code.lib.Implicits._
import code.lib.Session._

import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.StatefulSnippet
import net.liftweb.common.{Box, Full, Failure, Empty}
import net.liftweb.common.Box._

import scala.util.Try

class Signup extends StatefulSnippet {

  private var title: String = _
  private var nickname: String = _
  private var username: String = _
  private var accountType: Box[User.LoginService.Value] = Empty

  def dispatch = {
    case "render" => render
  }

  def parseAccountType(accountType: String) = Try {
    accountType match {
      case "Google" => User.LoginService.Google
      case "Flickr" => User.LoginService.Flickr
      case "ImgUr" => User.LoginService.ImgUr
    }
  }.toBox
  
  def verifyTitle: Box[String] = (Box !! title).filterNot(_.trim.isEmpty)
  def verifyNickname: Box[String] = (Box !! nickname).filterNot(_.trim.isEmpty)
  def verifyUsername: Box[String] = Try {

    if (username.toLowerCase.trim.isEmpty) {
      throw new Exception("使用者名稱不能是空白的")
    }

    if (username.toLowerCase.exists(ch => !ch.isLetterOrDigit && ch != '_')) {
      throw new Exception("使用者名稱只能有 [A-z][0-9][_]")
    }

    if (User.isUsernameTaken(username.toLowerCase).toBox.openOrThrowException("無法連結資料庫")) {
      throw new Exception("這個使用者名稱已經被註冊過了")
    }

    username.toLowerCase
  }.toBox

  def process(value: String) = {
    
    val user = for {
      email <- FirstLoginEmail.is ?~ "無法取得電子郵件地址，請重新登入一次"
      title <- verifyTitle ?~ "部落格標題不能是空白的"
      nickname <- verifyNickname ?~ "暱稱不能是空白的"
      username <- verifyUsername ?~ "使用者名稱不能是空白的"
      accountType <- this.accountType ?~ "無法取得登入服務種類"
      user <- User.createRecord.email(email).logtitle(title).nickname(nickname).username(username).service(accountType).saveTheRecord
    } yield {
      FirstLoginEmail.set(None)
      CurrentUser.set(Some(user))
      user
    }

    user match {
      case Full(user) => S.redirectTo(s"/user/${user.username.get}")
      case Empty => S.error("無法註冊，請稍候再試")
      case Failure(message, _, _) => S.error(message)
    }
  }

  def render = {
    "id=title" #> SHtml.text(title, title = _) &
    "id=nickname" #> SHtml.text(nickname, nickname = _) &
    "id=username" #> SHtml.text(username, username = _) &
    "id=accountType" #> SHtml.hidden(s => accountType = parseAccountType(s), S.param("accountType").getOrElse("")) &
    "type=submit" #> SHtml.onSubmit(process _)
  }
}
