package code.comet

import net.liftweb.http.NamedCometActorTrait

import net.liftweb.util.Helpers._
import net.liftweb.util.PassThru
import net.liftweb.http.CometActor
import net.liftweb.http.js.JsCmds._

case class Progress(percentage: Double)

class ProgressBar extends CometActor with NamedCometActorTrait {

  def render = PassThru

  override def lowPriority = {
    case Progress(percentage) => partialUpdate(Run(s"setProgress($percentage);"))
  }
}
