package code.snippet

import code.lib._
import code.model._
import code.lib.Session._
import code.lib.Implicits._

import net.liftweb.http.SHtml
import net.liftweb.http.S
import net.liftweb.util.Helpers._
import net.liftweb.util.ClearClearable
import net.liftweb.squerylrecord.RecordTypeMode._

class RandomPhoto {

  val size = S.attr("size").flatMap(asInt).getOrElse(10)
  val randomPhotos = Photo.randomList(size * 10).getOrElse(Nil).
                           filterNot(p => p.status.get == Photo.Status.Pending || p.status.get == Photo.Status.Hide).
                           take(size)

  def titleToChapaterSN(page: Page) = page.chapaters.zipWithIndex.toMap
  val pages = randomPhotos.map(_.blogID.get).distinct.map { id => 
    (id, inTransaction{Page(Blog.findByID(id).get)})
  }.toMap

  def getChapater(photo: Photo): Int =
  {
    val page = pages(photo.blogID.get)
    val pageTitle = page.chapaterToPhotos.
                mapValues(photos => photos.map(_.id)).
                filter { case(title, photoIDs) => photoIDs.contains(photo.id) }.
                keySet.mkString

    titleToChapaterSN(page)(pageTitle)
  }

  def render = {
    ClearClearable &
    "li" #> randomPhotos.map { photo =>
      
      val username = photo.blogAuthor.map(_.username).getOrElse("")
      val chapater = getChapater(photo)
      val linkURL = s"/user/${username}/log/${photo.blogID}/chapater/${chapater}#photo-${photo.id}"

      ".thumbImage [src]" #> photo.resizedURL(400, 400) &
      "a [href]" #> linkURL
    }
  }

}
