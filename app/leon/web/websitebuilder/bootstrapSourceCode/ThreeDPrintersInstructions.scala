import leon.collection._
import leon.webDSL.webDescription._
import leon.webDSL.webBuilding._
import implicits._

object ThreeDPrintersInstructions {
  // 3D printers
  case class ThreadSetup(
    kind: String,
    position: String
  )
  
  case class ThreeDPrinter(
    name: String,
    calibrated: Boolean,
    threads: List[ThreadSetup]
  )
  
  def renderCalibrated(d: ThreeDPrinter) =
    if(d.calibrated) "Your 3D printer has  already been calibrated."
    else "Please follow the instructions to put your printer."
  
  def renderThreadSetup(d: ThreadSetup): WebTree =
    TextElement("In the "+d.position+" printing head, put a "+d.kind+" thread.")
  
  def renderPrinter(d: ThreeDPrinter) = {
    <.ol(
      <.li(renderCalibrated(d))
    )( d.threads.map(renderThreadSetup))
  }
    
  val pp = List(
      ThreeDPrinter("Nikkon 3.0", calibrated=true,
          threads=List(ThreadSetup("plastic", ""))),
      ThreeDPrinter("Sumaprint 3000", calibrated=false,
          threads=List(ThreadSetup("plastic", "blue"), ThreadSetup("metallic", "grey"))))
  
  def main() = {
    WebPage(<.div(pp.map{ printer =>
      <.div(
        <.h1(printer.name + " mounting instructions"),
        renderPrinter(printer)
      ): WebTree
    }))
  }
}