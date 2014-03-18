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

  val pages = randomPhotos.map(_.blogID.get).distinct.map { id => 
    (id, inTransaction{Page(Blog.findByID(id).get)})
  }.toMap

  def getChapater(photo: Photo): Int =
  {
    val chapaters = pages(photo.blogID.get).chapaterToPhotos

    chapaters.view.zipWithIndex.filter { case((title, photos), i) =>
      photos.map(_.id) contains photo.id
    }.map(_._2).headOption.getOrElse(0)
  }

  def render = {
    ClearClearable &
    "li" #> randomPhotos.map { photo =>
      
      val username = photo.blogAuthor.map(_.username).getOrElse("")
      val chapater = getChapater(photo)
      val linkURL = s"/user/${username}/log/${photo.blogID}/chapater/${chapater}"

      ".thumbImage [src]" #> photo.resizedURL(400, 400) &
      "a [href]" #> linkURL
    }
  }

}
