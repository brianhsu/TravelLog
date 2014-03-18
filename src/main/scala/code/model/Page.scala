package code.model

import code.lib.Implicits._

import net.liftweb.common.Box
import scala.util.Try

case class Page(blog: Blog) {
  
  case class Chapater(
    title: String, mainPhotos: List[Photo], allPhotos: List[Photo], 
    currentChapater: Int, maxChapater: Int
  )

  lazy val photos = blog.orderedPhotos.getOrElse(Nil).filter(_.status.get != Photo.Status.Hide)
  lazy val chapaters = photos.map(_.chapater.get).distinct
  lazy val title = blog.title.toString
  lazy val authorNickname = blog.blogAuthor.map(_.nickname.get).getOrElse("")
  lazy val authorUsername = blog.blogAuthor.map(_.username.get).getOrElse("")
  lazy val chapaterToPhotos = photos.groupBy(_.chapater.get)
  lazy val isEmpty = photos.isEmpty

  def chapater(n: Int): Box[Chapater] = Try {

    if (n >= chapaters.size) {
      throw new IndexOutOfBoundsException(s"最大只到 $chapaters.size 頁，存取第 $n 頁")
    }

    val title = chapaters(n)
    val photosInChapater = chapaterToPhotos(title)

    Chapater(
      title, 
      mainPhotos = photosInChapater.filter(p => p.content.get.isDefined), 
      allPhotos = photosInChapater, 
      currentChapater = n, 
      maxChapater = chapaters.size
    )

  }.toBox

}
