package code.snippet

import code.lib.URLParam._
import code.lib.Implicits._

import code.model._

import net.liftweb.util.Helpers._
import net.liftweb.util.PassThru
import net.liftweb.http._

import net.liftweb.util.ClearClearable

import scala.xml.NodeSeq
import scala.xml.Text

class ShowPage(pageInfo: PageInfo)
{
  val page = pageInfo.page

  def render = {
    S.redirectTo(s"/user/${pageInfo.username}/log/${page.blog.id}/chapater/0")
    PassThru
  }
}
