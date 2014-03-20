import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

    val appName         = "leonWeb"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "org.scala-lang" % "scala-compiler" % "2.10.2",
      "com.h2database" % "h2" % "1.3.158",
      jdbc,
      anorm
    )

    val leon = RootProject(file("leon"))

    val main = play.Project(appName, appVersion, appDependencies).settings(
      // Add your own project settings here      
      scalaVersion := "2.10.2"
    ).dependsOn(leon)

}
