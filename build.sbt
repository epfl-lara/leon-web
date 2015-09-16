import sbt.Keys._
import sbt.Project.projectToRef

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
  "org.webjars" % "prettify" % "4-Mar-2013",
  "com.vmunier" %% "play-scalajs-scripts" % "0.2.2"
)

lazy val leon = RootProject(file("leon"))

scalaVersion := "2.11.7"

lazy val main = Project(appName, file(".")).enablePlugins(PlayScala).
  aggregate(aceJsProject, client).settings(
  version := appVersion,
  libraryDependencies ++= appDependencies,
  scalaJSProjects := Seq(client)
).dependsOn(leon, sharedJvm)

lazy val aceJsProject = RootProject(uri("https://github.com/MikaelMayer/scalajs-ace.git"))

lazy val client = (project in file("client")).settings(
  libraryDependencies ++= Seq("org.scala-js" %%% "scalajs-dom" % "0.8.0",
  "be.doeraene" %%% "scalajs-jquery" % "0.8.0",
  "com.github.japgolly.scalajs-react" %%% "core" % "0.9.2"),
  persistLauncher := true,
  jsDependencies +=
  "org.webjars" % "react" % "0.12.2" / "react-with-addons.js" commonJSName "React",
  skip in packageJSDependencies := false
).enablePlugins(ScalaJSPlugin, ScalaJSPlay).dependsOn(aceJsProject, sharedJs)

scalaVersion in client := "2.11.7"

lazy val shared = (crossProject.crossType(CrossType.Pure) in file("shared")).
  settings(scalaVersion := "2.11.7").
  jsConfigure(_ enablePlugins ScalaJSPlay)

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

watchSources := watchSources.value.filter ( source =>
  Seq("client-fastopt.js", "client-fastopt.js.map", "client-jsdeps.js"
  ,  "leon.js", "leon.js.map", "leon-deps.js"
  ).indexOf(source.getName) == -1 )

