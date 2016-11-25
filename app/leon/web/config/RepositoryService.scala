/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package config

import play.api.Play
import play.api.Play.current

import java.io.File
import java.nio.file.{Path, Paths}

object RepositoryService {

  case class BadConfig(msg: String) extends Exception(s"Bad configuration: $msg")

  case class Config(
    githubDir: File,
    tequilaDir: File
  )

  object Config {

    def create(globalDir: File, githubDir: Option[File], tequilaDir: Option[File]): Config = {
      val isValid = RootDir.isValid(globalDir)

      // As suggested by @MikaelMeyer: "Instead of throwing an exception, I would rather recommend to check first
      // if githubDir is set, and if so, remove the part ending with "/github" and set it as
      // the globalDir so that it can be used in the tequila directory."
      val rootDir = (isValid, githubDir) match {
          case (true, _) =>
            globalDir

          case (false, Some(dir)) =>
            new File(dir.getAbsolutePath.stripSuffix("/github"))

          case (false, None) =>
            throw new BadConfig(s"$globalDir does not exists")
        }

      def getPathOrResolveWith(file: Option[File], subDir: String) = 
        file.filter(RootDir.isValid).map(_.toPath).getOrElse(rootDir.toPath.resolve(subDir))

      val githubPath  = getPathOrResolveWith(githubDir, "github")
      val tequilaPath = getPathOrResolveWith(tequilaDir, "tequila")

      Config.create(githubPath.toFile, tequilaPath.toFile)
    }

    def create(githubDir: File, tequilaDir: File): Config = {
      Seq(githubDir, tequilaDir).filterNot(RootDir.isValid).foreach { dir =>
        throw new BadConfig(s"$dir does not exists")
      }

      Config(githubDir, tequilaDir)
    }

    def fromPlayAppConfig(config: play.api.Configuration): Config = {
      val globalDir  =
        config.getString("repositories.path")
          .map(new File(_))
          .getOrElse(throw new BadConfig("Missing key 'repositories.path'"))

      val githubDir  = config.getString("repositories.github.path").map(new File(_))
      val tequilaDir = config.getString("repositories.tequila.path").map(new File(_))

      Config.create(globalDir, githubDir, tequilaDir)
    }

    implicit def fromDefaultPlayAppConfig: Config = {
      fromPlayAppConfig(Play.configuration)
    }

  }

  private
  object RootDir {
    def isValid(d: File): Boolean = {
      d.exists && d.isDirectory && d.canWrite
    }
  }

}

