import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "leononline"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "org.scala-lang" % "scala-compiler" % "2.9.1",
      "com.h2database" % "h2" % "1.3.158",
      "junit" % "junit" % "4.8" % "test",
      "com.novocode" % "junit-interface" % "0.10-M3" % "test",
      "com.dongxiguo" %% "zero-log" % "0.1.2"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here      
    )

}
