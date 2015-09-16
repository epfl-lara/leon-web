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
  "org.webjars" % "prettify" % "4-Mar-2013"
)

lazy val leon = RootProject(file("leon"))

scalaVersion := "2.11.7"
    
lazy val jsProjects = Seq(client)
    
lazy val main = Project(appName, file(".")).enablePlugins(PlayScala).
  aggregate(aceJsProject, client).settings(
  version := appVersion,
  libraryDependencies ++= appDependencies,
  scalaJSProjects := jsProjects,
  pipelineStages := Seq(scalaJSProd)
).dependsOn(leon, shared)

lazy val aceJsProject = RootProject(uri("https://github.com/MikaelMayer/scalajs-ace.git"))

lazy val client = (project in file("client")).enablePlugins(ScalaJSPlugin, ScalaJSPlay).settings(
  libraryDependencies ++= Seq("org.scala-js" %%% "scalajs-dom" % "0.8.0",
  "be.doeraene" %%% "scalajs-jquery" % "0.8.0",
  "com.github.japgolly.scalajs-react" %%% "core" % "0.9.2"),
  jsDependencies +=
  "org.webjars" % "react" % "0.12.2" / "react-with-addons.js" commonJSName "React",
  unmanagedSourceDirectories in Compile += baseDirectory.value / "../shared/src",
  (fastOptJS in Compile) <<= (fastOptJS in Compile) map { result =>
    val inDir = file(".") / "client" / "target" / "scala-2.11"
    val outDir = file(".") / "public"
    val copy = Seq(
      ("client-fastopt.js", "leon.js"),
      ("client-fastopt.js.map", "leon.js.map"),
      ("client-jsdeps.js", "leon-deps.js")
    )
    val files = copy map (p => (inDir / p._1, outDir / p._2))
    IO.copy(files, true)
    result
  }
).dependsOn(aceJsProject)

scalaVersion in client := "2.11.7"
    
lazy val shared = (project in file("shared"))
scalaVersion in shared := "2.11.7"

watchSources := watchSources.value.filter ( source =>
  Seq("js-fastopt.js", "js-fastopt.js.map", "js-jsdeps.js"
  ).indexOf(source.getName) == -1 )

