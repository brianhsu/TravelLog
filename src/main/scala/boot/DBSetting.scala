package bootstrap.liftweb

import java.sql.DriverManager
import org.squeryl.Session
import net.liftweb.squerylrecord.SquerylRecord
import org.squeryl.adapters.PostgreSqlAdapter
import net.liftweb.util.Props

case class JDBCSetting(url: String, username: String, password: String)

object DBSetting {

  def envDBHost = Option(System.getProperty("DB_HOSTNAME"))
  def envDBName = Option(System.getProperty("DB_NAME"))
  def envDBUser = Option(System.getProperty("DB_USERNAME"))
  def envDBPass = Option(System.getProperty("DB_PASSWORD"))

  val jdbcSetting = for {
    host <- Props.get("DB_HOSTNAME") orElse envDBHost orElse Some("localhost")
    name <- Props.get("DB_NAME") orElse envDBName orElse Some("travellog")
    user <- Props.get("DB_USERNAME") orElse envDBUser orElse Some("username")
    pass <- Props.get("DB_PASS") orElse envDBPass orElse Some("password")
  } yield JDBCSetting(s"jdbc:postgresql://${host}/${name}", user, pass)

  Class.forName("org.postgresql.Driver")

  def connection = jdbcSetting.map { setting => 
    DriverManager.getConnection(setting.url, setting.username, setting.password)
  }.get

  def initDB() {
    SquerylRecord.initWithSquerylSession(Session.create(connection, new PostgreSqlAdapter))
  }
}

