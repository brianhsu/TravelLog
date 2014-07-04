name := "TravelLog"

version := "0.17"

scalaVersion := "2.10.3"

seq(webSettings :_*)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")

resolvers ++= Seq(
  "Bone" at "http://bone.twbbs.org.tw/ivy"
)

libraryDependencies ++= Seq(
  "javax.servlet" % "servlet-api" % "2.5" % "provided",
  "org.eclipse.jetty" % "jetty-webapp" % "8.0.1.v20110908" % "container",
  "org.scribe" % "scribe" % "1.3.5",
  "org.bone" %% "sphotoapi" % "0.4.2",
  "org.postgresql" % "postgresql" % "9.3-1100-jdbc41",
  "org.pegdown" % "pegdown" % "1.4.1",
  "com.sksamuel.scrimage" % "scrimage-core_2.10" % "1.3.11"
)

libraryDependencies ++= Seq(
  "net.liftweb" %% "lift-webkit" % "2.6-M2" % "compile->default",
  "net.liftweb" %% "lift-squeryl-record" % "2.6-M2",
  "net.liftmodules" %% "combobox" % "2.6-M2-0.7"
)

port in container.Configuration := 8081
