import sbt.Keys._
import sbt.Project.projectToRef

val appName         = "leonWeb"

val appVersion      = "1.0-SNAPSHOT"

val appDependencies = Seq(
  "joda-time" % "joda-time" % "2.1",
  "ws.securesocial" %% "securesocial" % "3.0-M3",
  "org.scala-lang" % "scala-compiler" % "2.11.6",
  "com.h2database" % "h2" % "1.3.158",
  "org.eclipse.jgit" % "org.eclipse.jgit.pgm" % "4.1.0.201509280440-r"
    exclude("javax.jms", "jms")
    exclude("com.sun.jdmk", "jmxtools")
    exclude("com.sun.jmx", "jmxri")
    exclude("org.sl4j", "slf4j-log4j12")
    exclude("log4j", "log4j"),
  jdbc,
  anorm,
  ws,
  // Web Libraries
  "org.webjars" % "ace" % "01.08.2014",
  "org.webjars" % "bootstrap" % "3.2.0",
  "org.webjars" % "jquery" % "2.1.1",
  "org.webjars" % "font-awesome" % "4.1.0",
  "org.webjars" % "octicons" % "3.1.0",
  "org.webjars" % "prettify" % "4-Mar-2013",
  "com.vmunier" %% "play-scalajs-scripts" % "0.2.2"
)

lazy val leon = RootProject(file("leon"))

EclipseKeys.skipProject in leon := true

scalaVersion := "2.11.7"

/****************************
 * Main play project *
 ****************************/

lazy val main = Project(appName, file(".")).enablePlugins(PlayScala).
  aggregate(aceJsProject, client).settings(
  version := appVersion,
  libraryDependencies ++= appDependencies,
  resolvers += Resolver.sonatypeRepo("releases"),
  scalaJSProjects := Seq(client)
).dependsOn(leon, sharedJvm)

/****************************
 * Client project (scalajs) *
 ****************************/

lazy val client = (project in file("client")).settings(
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.8.0",
    "be.doeraene" %%% "scalajs-jquery" % "0.8.0",
    "com.github.japgolly.scalajs-react" %%% "core" % "0.10.0",
    "org.monifu" %%% "monifu" % "1.0-RC3"
  ),
  persistLauncher := true,
  jsDependencies ++= Seq(
    "org.webjars.npm" % "react"     % "0.14.0" / "react-with-addons.js" commonJSName "React"    minified "react-with-addons.min.js",
    "org.webjars.npm" % "react-dom" % "0.14.0" / "react-dom.js"         commonJSName "ReactDOM" minified "react-dom.min.js" dependsOn "react-with-addons.js"
  ),
  skip in packageJSDependencies := false
).enablePlugins(ScalaJSPlugin, ScalaJSPlay).dependsOn(aceJsProject, sharedJs)

scalaVersion in client := "2.11.7"

watchSources := watchSources.value.filter ( source =>
  Seq("client-fastopt.js", "client-fastopt.js.map", "client-jsdeps.js"
  ,  "leon.js", "leon.js.map", "leon-deps.js"
  ).indexOf(source.getName) == -1 )
  
/*************************************
 * Shared code between js and server *
 *************************************/

lazy val shared = (crossProject.crossType(CrossType.Pure) in file("shared")).
  settings(scalaVersion := "2.11.7").
  jsConfigure(_ enablePlugins ScalaJSPlay)

lazy val sharedJvm = shared.jvm.settings(name := "sharedJvm", EclipseKeys.skipProject := true)
lazy val sharedJs = shared.js.settings(name := "sharedJs", EclipseKeys.skipProject := true)

/****************************
 * Ace for scala-js         *
 ****************************/

lazy val aceJsProject = RootProject(uri("https://github.com/MikaelMayer/scalajs-ace.git"))

lazy val scalajsAceLocalBase = SettingKey[File]("scalajsAceLocalBase",  "local base for scalajs ace project")
scalajsAceLocalBase := {
  loadedBuild.value.units(aceJsProject.build).localBase
}

/****************************
 * Eclipse project creation *
 ****************************/
  
EclipseKeys.skipParents in ThisBuild := false

lazy val updateEclipse = TaskKey[Unit]("updateEclipse", "Update eclipse files")

updateEclipse in main := {
  val jsp = baseDirectory.value / "client/.project"
  val jvmp = baseDirectory.value / ".project"
  val jsc = baseDirectory.value / "client/.classpath"
  val jvmc = baseDirectory.value / ".classpath"
  val jssettings = baseDirectory.value / "client/.settings/org.scala-ide.sdt.core.prefs"
  def replaceInFile(find: String, replace: String, file: File): Unit = {
    val content = IO.read(file)
    find.r.findFirstIn(content) match {
      case Some(_) =>
        val new_content = find.r.replaceAllIn(content, replace)
        IO.write(file, new_content)
        println("In " + (file.getParentFile().getName + "/" + file.getName) + ", replaced '"+find+"' by '"+replace+"'")
      case None =>
        println("Could not find in " + (file.getParentFile().getName + "/" + file.getName) + " the following '"+find+"'")
    }
  }
  val sep = (if(java.io.File.separator == "\\") "\\\\" else "/")
  val s = java.io.File.separator
  val replacements = Seq(
    ("""<name>client</name>""", """<name>leon-web-client</name>""",jsp),
    ("""<name>leonWeb</name>""", """<name>leon-web-server</name>""",jvmp),
    // Shorten the reference name for the "shared" directory.
    // Remove the output tag associated to them.
    ("""<classpathentry kind="src" path="/sharedJvm" exported="true" combineaccessrules="false"/>
  """, "", jvmc),
    ("""<classpathentry kind="src" path="/Leon" exported="true" combineaccessrules="false"/>
  """, "", jvmc),
    //("<link>\n *<name>.*leon-web-client.*</name>\n.*\n.*\n *</link>\n *","",jvmp),
    ("""<classpathentry kind="src" path="/sharedJs" exported="true" combineaccessrules="false"/>
 *""", "", jsc),
    /*("""<classpathentry output=".*" kind="src" path=".*leon-web-client-src-main.*"/>
  """, "", jvmc),*/
    /*("""<classpathentry kind="src" path="src"""+sep+"""test"""+sep+"""resources"/>
  ""","",jvmc),*/
    ("""<classpathentry kind="src" path="/Scala.js Ace" exported="true" combineaccessrules="false"/>
  ""","",jsc)
    // Set up the correct entry for the scalajs compiler
    // Remove duplicated entries in .project
    //("""(<link>(?:(?!</link>)(?:.|\r|\n))*</link>)\s*\1""","""$1""",jsp),//("""(<link>(?:(?!</link>)(?:.|\r|\n))*</link>)\s*\1""","""$1""",jvmp)
    // Remove dummy entries in .classpath
    //("""<classpathentry kind="src" path="shared\\main\\scala"/>\r?\n\s*""","", jsc),
    //("""<classpathentry kind="src" path="shared\\main\\scala"/>\r?\n\s*""","", jvmc)
  )
  replacements.foreach(t => replaceInFile(t._1, t._2, t._3))
  
  def addLinkedSourceFolder(projectFile: File, name: String, location: String): Unit = {
    replaceInFile("""<linkedResources>""", s"""<linkedResources>
    <link>
			<name>$name</name>
			<type>2</type>
			<location>$location</location>
		</link>""", projectFile)
  }
  def addClassPath(classPathFile: File, line: String): Unit = {
    replaceInFile("<classpath>", "<classpath>\n  "+line, classPathFile)
  }
  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")
  def doubleSep(path: String) = if(isWindows) "\\\\".r.replaceAllIn(path, "\\\\\\\\") else path
  
  val scalaJsAceFolder = doubleSep(scalajsAceLocalBase.value.getAbsolutePath())
  val srcmainscala = sep + "src" + sep + "main" + sep + "scala"
  
  addClassPath(jsc, s"""
  <classpathentry kind="lib" path="$scalaJsAceFolder"""+sep+"target"+sep+"scala-2.11"+sep+"classes"+s"""" sourcepath="$scalaJsAceFolder$srcmainscala"/>""")
  
  def sepNormalize(path: String) = if(isWindows) "\\\\".r.replaceAllIn(path, "/") else path
  
  val sharedSourceFolder = sepNormalize(baseDirectory.value.getAbsolutePath()) + sep + "shared" + srcmainscala
  
  for((proj, clas) <- Seq((jvmp, jvmc),(jsp,jsc))) {
    addLinkedSourceFolder(proj, "shared-main-scala", sharedSourceFolder)
    addClassPath(clas, """<classpathentry kind="src" path="shared-main-scala"/>""")
  }
  
  if(isWindows) { // Just set up the correct jars.
    val content = IO.read(jvmc)
    for(library <- Seq("cafebabe", "vanuatoo")) {
      val Re = ("<classpathentry kind=\"lib\" path=\"(.*)"+sep+"(\\d\\d)"+sep+"("+library+".*.jar)\"/>").r.unanchored
      content match {
        case Re(initpath,folder,jar) => IO.copyFile(new File(initpath + sep + "common" + sep + jar), new File(initpath + sep + folder + sep + jar))
        case _ => println("Impossible to fix leon")
      }
    }
  }
  val leonFolder = doubleSep((baseDirectory.value / "leon").getAbsolutePath())
    
  // Append leon libraries at the end of vanuatoo (sub leon library).
  val toFind = """(<classpathentry kind="lib" path=".*leon.*vanuatoo_2.11-0.1.jar"/>)"""
  val toReplace = "$1\n\t<classpathentry kind=\"lib\" path=\"" +
    leonFolder +
    sep + "target" + sep + "scala-2.11" + sep +
    "classes\" sourcepath=\""+s"$leonFolder$srcmainscala"+"\"/>"
  
  replaceInFile(toFind, toReplace, jvmc)
}

addCommandAlias("eclipseCreate", ";eclipse;updateEclipse")
