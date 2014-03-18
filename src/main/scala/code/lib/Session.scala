package code.lib

import code.model.User
import net.liftweb.http.SessionVar

object Session {
  object CurrentUser extends SessionVar[Option[User]](None)
  object FirstLoginEmail extends SessionVar[Option[String]](None)
}

