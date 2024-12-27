import hevs.graphics.FunGraphics
import java.awt.event.{MouseEvent, MouseListener}
import java.awt.Color
import scala.util.Random



class Grid(val width: Int, val height: Int, val nbOfElement: Int, val display: FunGraphics) {


  val box: Array[Array[Int]] = Array.ofDim(nbOfElement, nbOfElement)
  val margin: Int = 40
  val fontSize: Int = 14
  val caseWidth: Int = (width - margin * 2) / nbOfElement
  val boxWidth: Int = caseWidth * nbOfElement
  val possibilities: Array[Int] = Array(1, 2, 3, 4, 5)
  var iCount = 0
  var jCount = 0

  def initializeElements(arr: Array[Int]): Unit = {
    for (i <- box.indices) {
      for (j <- box(i).indices) {
        box(i)(j) = arr(Random.nextInt(arr.length))
      }
    }
  }

  def drawElements(): Unit = {
    for (i <- margin to boxWidth by caseWidth) {
      jCount = 0
      for (j <- margin to boxWidth by caseWidth) {
        display.drawString(i, j, box(iCount)(jCount).toString, new Color(0, 0, 0), fontSize)
        jCount += 1
      }
      iCount += 1
    }
  }

  def doWeHaveAMatch(): Boolean = {
    val impossibleValue = 99
    var lastMatch: Int = impossibleValue
    var matchCount: Int = 0

    //check vertically
    for (i <- box.indices) {
      for (j <- box(i).indices) {
        if (box(i)(j) == lastMatch) {

          matchCount += 1
          if (matchCount >= 2) return true
        }
        else {
          matchCount = 0
          lastMatch = box(i)(j)

        }
      }
      lastMatch = impossibleValue
      matchCount = 0
    }
    //check horizontally
    for (j <- box.indices) {
      for (i <- box(j).indices) {
        if (box(i)(j) == lastMatch) {
          matchCount += 1
          if (matchCount >= 2) return true

        }
        else {
          matchCount = 0
          lastMatch = box(i)(j)

        }
      }
      lastMatch = impossibleValue
      matchCount = 0
    }


    return false
  }

  /*def getMatchPosition():Array[Array[Int]]={

  }*/

  initializeElements(possibilities)
  drawElements()
  println(doWeHaveAMatch())

  display.addMouseListener(new MouseListener {
    override def mouseClicked(e: MouseEvent): Unit = {

    }

    override def mousePressed(e: MouseEvent): Unit = {
      val clickX = e.getX
      val clickY = e.getY

      val caseNumberX = (clickX - margin) / caseWidth
      val caseNumberY = (clickY - margin) / caseWidth

      /*mySelection.update(new Position(caseNumberX, caseNumberY))
      var sameNeighbours: Array[Position] = mySelection.getHorizontalNeighbours(box, new Position(caseNumberX, caseNumberY))
      for (i <- sameNeighbours) {
        println("neighoubours", i.x, i.y)
      }
      */


      val cellX: Int = caseNumberX * caseWidth + margin
      val cellY: Int = caseNumberY * caseWidth + margin - fontSize

      display.setColor(Color.BLUE)
      display.drawFillRect(cellX, cellY, caseWidth, caseWidth)

    }

    override def mouseReleased(e: MouseEvent): Unit = {}

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mouseExited(e: MouseEvent): Unit = {}
  })

}
