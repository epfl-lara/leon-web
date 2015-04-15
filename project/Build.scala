import sbt._
import Keys._
import play.Play.autoImport._
import PlayKeys._

object ApplicationBuild extends Build {

    val appName         = "leonWeb"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "joda-time" % "joda-time" % "2.1",
      "org.scala-lang" % "scala-compiler" % "2.11.6",
      "com.h2database" % "h2" % "1.3.158",
      jdbc,
      anorm,

      // Web Libraries
      "org.webjars" % "ace" % "01.08.2014",
      "org.webjars" % "bootstrap" % "3.2.0",
      "org.webjars" % "jquery" % "2.1.1",
      "org.webjars" % "font-awesome" % "4.1.0",
      "org.webjars" % "prettify" % "4-Mar-2013"
    )

    val leon = RootProject(file("leon"))

    val main = Project(appName, file(".")).enablePlugins(play.PlayScala).settings(
      version := appVersion,
      libraryDependencies ++= appDependencies,
      scalaVersion := "2.11.6"
    ).dependsOn(leon)

}
