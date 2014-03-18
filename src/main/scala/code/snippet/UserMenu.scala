package code.snippet

import code.lib.Session._

import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml

import scala.xml.NodeSeq
import scala.xml.Text

class UserMenu {

  def isLoggedIn = CurrentUser.is.isDefined

  def showUserMenu = {
    ".avatar [src]" #> CurrentUser.is.map(_.avatarURL).getOrElse("") &
    "#avatarLink [href]" #> CurrentUser.is.map(u => s"/user/${u.username}").getOrElse("#") &
    ".logout" #> SHtml.link("/", () => CurrentUser.is.foreach(_.logout()), Text("登出"))
  }

  def render = isLoggedIn match {
    case true  => "#loginMenu" #> NodeSeq.Empty & showUserMenu
    case false => "#userMenu"  #> NodeSeq.Empty
  }
}
