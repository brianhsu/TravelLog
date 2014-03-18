package code.lib

import code.model.User
import code.lib.Session._

import net.liftweb.http.S

object UserCheck {
  
  def redirectIfNotSameUser(user: User) {
    if (user.id != CurrentUser.get.map(_.idField.get).getOrElse(-1)) {
      S.redirectTo(s"/user/${user.username.get}")
    }
  }

}

