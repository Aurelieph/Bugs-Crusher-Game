import hevs.graphics.FunGraphics

import java.awt.Color
import java.awt.event.{MouseEvent, MouseListener}
import scala.util.Random

class Position(var a: Int, var b: Int) {
  var x: Int = a
  var y: Int = b

  def this() = this(-1, -1)
}


object Base extends App {
  val width: Int = 600
  val height: Int = 800
  val nbOfElement: Int = 9
  val display: FunGraphics = new FunGraphics(width, height, "Our Super Game")

  val box: Array[Array[Int]] = Array.ofDim(nbOfElement, nbOfElement)

  val margin: Int = 40
  val caseWidth: Int = (width - margin * 2) / nbOfElement
  println(caseWidth)
  val boxWidth: Int = caseWidth * nbOfElement
  val possibilities: Array[Int] = Array(1, 2, 3, 4, 5)
  val fontSize: Int = 8
  var mySelection = new Selections()

  def initializeElements(arr: Array[Int]): Unit = {
    for (i <- box.indices) {
      for (j <- box(i).indices) {
        box(i)(j) = arr(Random.nextInt(arr.length))
      }
    }
  }

  def displayFillBox(arr: Array[Array[Int]]): Unit = {
    var iCount = 0
    var jCount = 0
    for (i <- margin to boxWidth by caseWidth) {
      jCount = 0
      for (j <- margin to boxWidth by caseWidth) {
        display.drawString(i, j, arr(iCount)(jCount).toString, new Color(0, 0, 0), fontSize)
        jCount += 1
      }
      iCount += 1
    }
  }

  class Selections {
    var firstSelection: Position = new Position()
    var secondSelection: Position = new Position()

    //TODO make sure we will manage the deselection once the resolution of the action will occur.
    def update(position: Position): Unit = {
      if (firstSelection.x == -1 || firstSelection.y == -1) {
        secondSelection = new Position
        firstSelection = position
      }
      else {
        secondSelection = position
      }
      //println(firstSelection.x, firstSelection.y, secondSelection.x, secondSelection.y)
    }

    def getValueElement(arr: Array[Array[Int]], position: Position): Int = {
      arr(position.x)(position.y)
    }

    //TODO update
    def getHorizontalNeighbours(arr: Array[Array[Int]]): Int = {
      val leftLimit: Int = Math.max(firstSelection.x - 2, 0)
      val rightLimit: Int = Math.min(firstSelection.x + 2, width)

      for (i <- leftLimit to rightLimit) {

      }

      0
    }

  }


  initializeElements(possibilities)

  displayFillBox(box)

  display.addMouseListener(new MouseListener {
    override def mouseClicked(e: MouseEvent): Unit = {
      val clickX = e.getX
      val clickY = e.getY

      val caseNumberX = (clickX - margin) / caseWidth
      val caseNumberY = (clickY - margin) / caseWidth

      mySelection.update(new Position(caseNumberX, caseNumberY))

      val cellX: Int = caseNumberX * caseWidth + margin
      val cellY: Int = caseNumberY * caseWidth + margin - fontSize

      display.setColor(Color.BLUE)
      display.drawFillRect(cellX, cellY, caseWidth, caseWidth)
    }

    override def mousePressed(e: MouseEvent): Unit = {


    }

    override def mouseReleased(e: MouseEvent): Unit = {}

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mouseExited(e: MouseEvent): Unit = {}
  })


}
