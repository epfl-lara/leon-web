package leon.web
package workers

import akka.actor._

import akka.pattern._

import java.io.File
import java.io.PrintWriter
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Try

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import play.api.Play.current
import play.api._
import play.api.libs.concurrent._
import play.api.libs.iteratee._

import leon.frontends.scalac._
import leon.purescala.Definitions.Program
import leon.utils.InterruptManager
import leon.utils.PreprocessingPhase
import leon.utils.TemporaryInputPhase

import leon.web.models._
import leon.web.services._
import leon.web.shared.Module
import leon.web.shared.{Action, RepositoryState}
import leon.web.stores._
import leon.web.utils.String._
import leon.web.websitebuilder.memory.Memory

class CompilationWorker(
  session: ActorRef,
  im: InterruptManager,
  channel: Concurrent.Channel[Array[Byte]]
) extends WorkerActor(session, im) {

  override val reporter = new CompilingWSReporter(channel)

  import ConsoleProtocol._

  import context.dispatcher

  def shouldRecompile(cstate: CompilationState, repoState: Option[RepositoryState], code: String): Boolean = {
    cstate.repoState != repoState || cstate.code != Some(code)
  }

  import shared._
  import shared.messages.{ DoCancel => MDoCancel, _ }

  def resolveFile(repoState: Option[RepositoryState], user: Option[models.User]): Option[File] = {
    for {
      s            <- repoState
      u            <- user
      wc           <- RepositoryService.getWorkingCopy(u, s.repo.desc)
      (_, _, path) <- wc.getFile(s.branch, s.file)
    }
    yield wc.path.toPath.resolve(path).toFile
  }

  def loadAllFiles(repoState: Option[RepositoryState], user: Option[models.User]): Option[List[String]] = {
    for {
      s     <- repoState
      if s.asProject
      u     <- user
      wc    <- RepositoryService.getWorkingCopy(u, s.repo.desc)
      files = wc.getFiles(s.branch).getOrElse(Seq()).toList
    } yield {
      files.filter(_.extension === "scala").map(new File(_)).map { f =>
        wc.path
          .toPath
          .resolve(f.getPath)
          .toFile
          .getAbsolutePath
      }
    }
  }

  def postConditionHasQMark(program: Program, savedFile: File): Boolean = {
    program.definedFunctions.exists { funDef =>
      funDef.getPos.file != null && savedFile != null &&
      funDef.getPos.file.getAbsolutePath == savedFile.getAbsolutePath &&
      (funDef.postcondition match {
        case Some(postCondition) =>
        import leon.purescala._
        import Expressions._
        ExprOps.exists {
          case FunctionInvocation(callee, _) =>
            leon.purescala.DefOps.fullName(callee.fd)(program) === "leon.invariant.?"
          case _ =>
            false
        }(postCondition)
        case None => false
      })
    }
  }

  def receive = {

    case DoCancel =>
      sender ! Cancelled(this)

    case USetCommandFlags(_) =>
      // do nothing, as those flags don't affect compilation

    case OnUpdateCode(_) =>
      // do nothing, as we're the ones who just triggered that event

    case Compile(lastCompilationState, code, user, repoState, requestId, invariantEnabled) =>
      Memory.clearClarificationSession()

      if (!shouldRecompile(lastCompilationState, repoState, code)) {
          val cstate = lastCompilationState.copy(requestId = Some(requestId))
          session ! DispatchTo(WebsiteBuilder, OnUpdateCode(cstate))
          event(HCompilation(cstate.compResult))
      }
      else {
        clientLog("Compiling...")
        logInfo(s"Code updated:\n$code")

        val file      = resolveFile(repoState, user)
        val savedFile = saveCode(code, file)

        resetLeonContextOptions()

        val compReporter = new CompilingWSReporter(channel)
        val compContext  = leon.Main.processOptions(Nil).copy(reporter = compReporter)

        val optProgram = try {
          val pipeline = ExtractionPhase andThen (new PreprocessingPhase(false))

          val repoFiles = loadAllFiles(repoState, user)
          val runFiles  = repoFiles.getOrElse(savedFile.getAbsolutePath :: Nil)

          val (ctx, program) = pipeline.run(compContext, runFiles)

          val notifyTerminationChecker = invariantEnabled && postConditionHasQMark(program, savedFile)

          compReporter.terminateIfError

          Some((program, notifyTerminationChecker))
        }
        catch {
          case e: java.nio.channels.ClosedChannelException =>
            logInfo("Channel closed")
            None

          case t: Throwable =>
            logInfo("Failed to compile and/or extract " + t)
            None
        }

        val cstate = optProgram match {
          case Some((program, _)) =>

            val cstate = CompilationState(
              optProgram = Some(program),
              code       = Some(code),
              compResult = "success",
              requestId  = Some(requestId),
              wasLoop    = Set(),
              repoState  = repoState,
              savedFile  = Some(savedFile.getName())
            )

            event(HCompilation("success"))

            clientLog("Compilation successful!")

            notifyMainOverview(cstate)

            cstate

          case None =>
            for ((l,e) <- compReporter.errors) {
              logInfo(s"  ${e mkString "\n  "}")
            }

            clientLog("Compilation failed!")
            event(HCompilation("failure"))

            CompilationState.failure(
              code, repoState, Some(savedFile.getName())
            )
        }

        val notifyTerminationChecker = optProgram.map(_._2).getOrElse(false)

        session ! CompilationDone(
          cstate,
          optProgram.map(_._1),
          notifyTerminationChecker
        )

        val annotations = {
          compReporter.errors.map{ case (l,e) =>
            shared.CodeAnnotation(l, 0, e.mkString("\n"), shared.CodeAnnotationError)
          }.toSeq ++
          compReporter.warnings.map{ case (l,e) =>
            shared.CodeAnnotation(l, 0, e.mkString("\n"), shared.CodeAnnotationWarning)
          }.toSeq
        }.filter(_.line >= 0)

        notifyAnnotations(annotations)
      }

    case msg =>
      clientLog("CompilationWorker received an unknown message: " + msg)
  }

  def notifyMainOverview(cstate: CompilationState): Unit = {

    def decodeName(name: String): String = {
      scala.reflect.NameTransformer.decode(name).replaceAll("\\$", ".")
    }

    if (cstate.isCompiled) {
      val facts: Map[String, OverviewFunction] = (for (fd <- cstate.functions) yield {
        fd.id.name -> OverviewFunction(fd.id.name, decodeName(fd.id.name), fd.getPos.line, fd.getPos.col)
      }).toMap

      event(HUpdateOverview(facts))
    }

  }

  def saveCode(code: String, file: Option[File] = None): File = file match {
    case None =>

      val format   = DateTimeFormat.forPattern("YYYY-MM-dd_HH-mm-ss.SS")
      val dateTime = new DateTime().toString(format)
      val file     = new File(s"logs/inputs/$dateTime.scala")

      saveCode(code, Some(file))

    case Some(file) =>
      val w = new PrintWriter(file , "UTF-8")

      try {
        w.print(code)
      } finally {
        w.close
      }

      file
  }

  def notifyAnnotations(annotations: Seq[shared.CodeAnnotation]): Unit = {
    event(HEditor(annotations = Some(annotations.toArray)))
  }

}
