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
  val boxWidth: Int = caseWidth * nbOfElement
  val possibilities: Array[Int] = Array(1, 2, 3, 4, 5)
  val fontSize: Int = 14
  val noPositionValue: Int = -1
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
      if (firstSelection.x == noPositionValue || firstSelection.y == noPositionValue) {
        secondSelection = new Position
        firstSelection = position
      }
      else {
        secondSelection = position
      }
    }

    //TODO update
    def getHorizontalNeighbours(arr: Array[Array[Int]], position: Position): Array[Position] = {
      val currentValue: Int = arr(position.x)(position.y)
      val leftNeighboursCoordinates: Array[Position] = new Array(2)
      val rightNeighboursCoordinates: Array[Position] = new Array(2)
      var sameLeftValue: Boolean = true
      var sameRightValue: Boolean = true
      for (i <- 0 to 1) {
        val leftPosition: Int = position.x - i - 1
        if (leftPosition >= 0 && arr(leftPosition)(position.y) == currentValue && sameLeftValue) {
          leftNeighboursCoordinates(i) = new Position(leftPosition, position.y)
        }
        else {
          leftNeighboursCoordinates(i) = new Position()
          sameLeftValue = false
        }
        val rightPosition: Int = position.x + i + 1
        if (rightPosition <= nbOfElement - 1 && arr(rightPosition)(position.y) == currentValue && sameRightValue) {
          rightNeighboursCoordinates(i) = new Position(rightPosition, position.y)
        }
        else {
          rightNeighboursCoordinates(i) = new Position()
          sameRightValue = false
        }

      }

      val tempArray: Array[Position] = new Array(4)
      var count: Int = 0
      for (i <- rightNeighboursCoordinates.indices) {
        if (rightNeighboursCoordinates(i).x != noPositionValue) {
          tempArray(count) = rightNeighboursCoordinates(i)
          count += 1
        }
        if (leftNeighboursCoordinates(i).x != noPositionValue) {
          tempArray(count) = leftNeighboursCoordinates(i)
          count += 1
        }
      }
      val (coordinatesResult, empty) = tempArray.splitAt(count)
      for (i <- coordinatesResult.indices) {
        println(coordinatesResult(i).x, coordinatesResult(i).y)

      }

      coordinatesResult
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
      var sameNeighbours: Array[Position] = mySelection.getHorizontalNeighbours(box, new Position(caseNumberX, caseNumberY))
      for (i <- sameNeighbours) {
        println("neighoubours", i.x, i.y)
      }

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
