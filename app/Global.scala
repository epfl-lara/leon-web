import play.api._
import leon.web.models.Permalink

object Global extends GlobalSettings {
  override def onStart(app: Application) {
    Permalink.setup()
  }
}
