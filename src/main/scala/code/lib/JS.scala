package code.lib

import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds._

object FadeOutAndRemove
{
  def byClassName(className: String, timespan: Int = 500): JsCmd = 
  {
    JsRaw("""
      $('.%s').fadeOut(%d, function() {
        $('.%s').remove()
      })
    """.format(className, timespan, className))
  }

  def apply(id: String, timespan: Int = 500): JsCmd = {
    JsRaw("""
      $('#%s').fadeOut(%d, function() { 
        console.log("remove...")
        $('#%s').remove() 
      })
    """.format(id, timespan, id))
  }
}

