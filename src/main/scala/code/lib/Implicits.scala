package code.lib

import scala.util.{Try, Success, Failure}
import net.liftweb.common.{Box, Empty, Full}
import org.pegdown.PegDownProcessor

import scala.xml.NodeSeq
import scala.xml.Unparsed

import scala.reflect.runtime.universe._
import org.pegdown.Extensions._

object Implicits {

  object MarkdownParser {
    val parser = new PegDownProcessor(ALL|SUPPRESS_ALL_HTML)
  }
  
  class MarkdownParser(in: String) {
    def markdownToHTML: Try[NodeSeq] = Try { 
      Unparsed(MarkdownParser.parser.markdownToHtml(in))
    }
  }

  class TryToBox[T](t: Try[T]) {

    def toBox: Box[T] = t match {
      case Success(result) => Full(result)
      case Failure(error) => net.liftweb.common.Failure(error.getMessage, Full(error), Empty)
    }

  }

  class TryOptionToBox[T](t: Try[Option[T]]) {

    def oToBox: Box[T] = t match {
      case Success(Some(result)) => Full(result)
      case Success(None) => Empty
      case Failure(error) => net.liftweb.common.Failure(error.getMessage, Full(error), Empty)
    }

  }

  implicit def convertToBox1[T](t: Try[T]): TryToBox[T] = new TryToBox(t)
  implicit def convertToBox2[T](t: Try[Option[T]]): TryOptionToBox[T] = new TryOptionToBox(t)
  implicit def convertToMarkdown(in: String) = new MarkdownParser(in)

}
