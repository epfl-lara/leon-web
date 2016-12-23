import sbt.Keys._
import sbt.Project.projectToRef

val appName         = "leonWeb"

val appVersion      = "1.0-SNAPSHOT"

val appDependencies = Seq(
  "joda-time" % "joda-time" % "2.3",
  "ws.securesocial" %% "securesocial" % "3.0-M3",
  "com.lihaoyi" %% "sourcecode" % "0.1.1",
  "org.scala-lang" % "scala-compiler" % "2.11.6",
  "com.h2database" % "h2" % "1.3.158",

  "org.eclipse.jgit" % "org.eclipse.jgit.pgm" % "4.1.0.201509280440-r"
    exclude("javax.jms", "jms")
    exclude("com.sun.jdmk", "jmxtools")
    exclude("com.sun.jmx", "jmxri")
    exclude("org.slf4j", "slf4j-log4j12")
    exclude("log4j", "log4j"),

  jdbc,
  anorm,
  ws,

  // Web Libraries
  "org.webjars" % "ace" % "01.08.2014",
  "org.webjars" % "bootstrap" % "3.2.0",
  "org.webjars" % "jquery" % "2.1.1",
  "org.webjars" % "jquery-ui" % "1.11.4",
  "org.webjars" % "font-awesome" % "4.7.0",
  "org.webjars" % "octicons" % "3.1.0",
  "org.webjars" % "prettify" % "4-Mar-2013",
  "com.vmunier" %% "play-scalajs-scripts" % "0.2.2"
)

excludeDependencies ++= Seq(
  "org.slf4j" % "slf4j-jdk14",
  "org.slf4j" % "slf4j-nop",
  "org.slf4j" % "slf4j-log4j12"
)

val compilerOptions = Seq(
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xlint:-missing-interpolator",
  // "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Xfuture"

  // only enabled in main as it doesn't play well
  // with ScalaJS js.native
  // "-Ywarn-dead-code"

  // only enabled in client because of
  // https://github.com/playframework/playframework/issues/5216
  // "-Ywarn-unused-import"
)

lazy val leon = RootProject(file("leon"))

scalaVersion := "2.11.8"


/****************************
 * Main play project        *
 ****************************/

lazy val main = Project(appName, file(".")).enablePlugins(PlayScala).
  aggregate(client).settings(
  version := appVersion,
  libraryDependencies ++= appDependencies,
  resolvers += Resolver.sonatypeRepo("releases"),
  scalacOptions ++= compilerOptions ++ Seq("-Ywarn-dead-code"),
  javaOptions in run ++= Seq("-Xms100M"),
  scalaJSProjects := Seq(client),
  pipelineStages := Seq(scalaJSProd)
).dependsOn(leon, sharedJvm)

/****************************
 * Client project (scalajs) *
 ****************************/

lazy val client = (project in file("client")).settings(
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.0",
    "be.doeraene" %%% "scalajs-jquery" % "0.9.0",
    "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1",
    "com.github.japgolly.scalajs-react" %%% "extra" % "0.11.1",
    "com.github.japgolly.scalacss" %%% "ext-react" % "0.4.1",
    "org.monifu" %%% "monifu" % "1.0-RC4",
    "com.scalawarrior" %%% "scalajs-ace" % "0.0.2",
    "com.lihaoyi" %%% "upickle" % "0.3.6"
  ),
  resolvers += "amateras-repo" at "http://amateras.sourceforge.jp/mvn/",
  scalacOptions ++= compilerOptions ++ Seq("-Ywarn-unused-import"),
  persistLauncher := true,
  jsDependencies ++= Seq(
    "org.webjars" % "jquery" % "2.1.3" / "2.1.3/jquery.js",
    "org.webjars" % "jquery-ui" % "1.11.4" / "jquery-ui.js" dependsOn "jquery.js",
    "org.webjars.bower" % "react" % "15.0.1" / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
    "org.webjars.bower" % "react" % "15.0.1" / "react-dom.js" minified "react-dom.min.js" dependsOn "react-with-addons.js" commonJSName "ReactDOM",
    "org.webjars.bower" % "react" % "15.0.1" / "react-dom-server.js" minified  "react-dom-server.min.js" dependsOn "react-dom.js" commonJSName "ReactDOMServer"
  ),
  skip in packageJSDependencies := false
).enablePlugins(ScalaJSPlugin, ScalaJSPlay).dependsOn(sharedJs)

watchSources := watchSources.value.filter ( source =>
  Seq("client-fastopt.js", "client-fastopt.js.map", "client-jsdeps.js"
  ,  "leon.js", "leon.js.map", "leon-deps.js"
  ).indexOf(source.getName) == -1 )
  
/*************************************
 * Shared code between js and server *
 *************************************/

lazy val shared = (crossProject.crossType(CrossType.Pure) in file("shared")).
  settings(
    scalaVersion := "2.11.8",
    libraryDependencies ++= Seq(
    "me.chrons" %%% "boopickle" % "1.1.3"),
    scalaSource in Compile := baseDirectory.value / "../../leon/library").
  jsConfigure(_ enablePlugins ScalaJSPlay)

lazy val sharedJvm = shared.jvm.settings(name := "sharedJvm")
lazy val sharedJs = shared.js.settings(name := "sharedJs")
