import hevs.graphics.FunGraphics
import java.awt.event.{MouseEvent, MouseListener}
import java.awt.Color
import scala.util.Random

class Grid(val width: Int, val height: Int, val nbOfElement: Int, val display: FunGraphics) {


  val box: Array[Array[Element]] = Array.ofDim(nbOfElement, nbOfElement)
  val margin: Int = 40
  val fontSize: Int = 14
  val caseWidth: Int = (width - margin * 2) / nbOfElement
  val boxWidth: Int = caseWidth * nbOfElement
  val possibilities: Array[Int] = Array(1, 2, 3, 4, 5)


  def initializeElements(arr: Array[Int]): Unit = {
    for (i <- box.indices) {
      for (j <- box(i).indices) {
        box(i)(j) = new Element(arr(Random.nextInt(arr.length)), i, j)

      }
    }
  }

  def drawElements(): Unit = {
    var iCount = 0
    var jCount = 0
    for (i <- margin to boxWidth by caseWidth) {
      jCount = 0
      for (j <- margin to boxWidth by caseWidth) {
        display.drawString(i, j, box(iCount)(jCount).value.toString, new Color(0, 0, 0), fontSize)
        jCount += 1
      }
      iCount += 1
    }
  }

  def drawElementsTest(): Unit = {
    var iCount = 0
    var jCount = 0
    for (i <- margin to boxWidth by caseWidth) {
      jCount = 0
      for (j <- margin to boxWidth by caseWidth) {
        display.drawString(i + 10, j, box(iCount)(jCount).isPartOfMatch.toString, new Color(0, 0, 0), fontSize)
        display.drawString(i + 10, j + 10, box(iCount)(jCount).countVerticalMoves.toString, new Color(0, 0, 0), fontSize)
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
        if (box(i)(j).value == lastMatch) {

          matchCount += 1
          if (matchCount >= 2) return true
        }
        else {
          matchCount = 0
          lastMatch = box(i)(j).value

        }
      }
      lastMatch = impossibleValue
      matchCount = 0
    }
    //check horizontally
    for (j <- box.indices) {
      for (i <- box(j).indices) {
        if (box(i)(j).value == lastMatch) {
          matchCount += 1
          if (matchCount >= 2) return true

        }
        else {
          matchCount = 0
          lastMatch = box(i)(j).value

        }
      }
      lastMatch = impossibleValue
      matchCount = 0
    }


    false
  }

  def identifyMatch(): Boolean = {
    val impossibleValue = 99
    var lastMatch: Int = impossibleValue
    var matchCount: Int = 0
    var isMatch: Boolean = false

    //go through vertically
    for (i <- box.indices) {
      for (j <- box(i).indices) {
        if (box(i)(j).value == lastMatch) {
          matchCount += 1
          if (matchCount >= 2) {
            isMatch = true
            for (k <- 0 to matchCount) {
              box(i)(j - k).isPartOfMatch = true
            }
          }
        }
        else {
          matchCount = 0
          lastMatch = box(i)(j).value

        }
      }
      lastMatch = impossibleValue
      matchCount = 0
    }
    //check horizontally
    matchCount = 0
    lastMatch = impossibleValue
    for (j <- box.indices) {
      for (i <- box(j).indices) {
        if (box(i)(j).value == lastMatch) {
          matchCount += 1
          if (matchCount >= 2) {
            isMatch = true
            for (k <- 0 to matchCount) {
              box(i - k)(j).isPartOfMatch = true
            }
          }

        }
        else {
          matchCount = 0
          lastMatch = box(i)(j).value

        }
      }
      lastMatch = impossibleValue
      matchCount = 0
    }


    isMatch
  }

  // Update the countVerticalMoves value to indicate how many positions each element will drop down after the matches.
  def identifyVerticalMoves(): Unit = {
    for (i <- box.indices) {
      for (j <- box(i).indices) {
        if (j == 0) {
          if (box(i)(nbOfElement - j - 1).isPartOfMatch) {
            box(i)(nbOfElement - j - 1).countVerticalMoves = 1
          }
        }
        else if (box(i)(nbOfElement - j - 1).isPartOfMatch) {
          box(i)(nbOfElement - j - 1).countVerticalMoves = box(i)(nbOfElement - j).countVerticalMoves + 1
        }
        else {
          box(i)(nbOfElement - j - 1).countVerticalMoves = box(i)(nbOfElement - j).countVerticalMoves
        }
      }
    }
  }

  def resolveMatches():Unit = {
    for (i<- box.indices){
      for(j<- box(i).indices){
        if(box(i)(j).isPartOfMatch){
          display.setColor(Color.white)
          display.drawFillRect(margin+box(i)(j).x*caseWidth, margin-fontSize+box(i)(j).y*caseWidth, caseWidth, caseWidth)

        }
      }
    }
  }
  //def cleanGrid


  initializeElements(possibilities)


  drawElements()
  identifyMatch()
  identifyVerticalMoves()
  drawElementsTest()

  display.addMouseListener(new MouseListener {
    override def mouseClicked(e: MouseEvent): Unit = {

    }

    override def mousePressed(e: MouseEvent): Unit = {
      val clickX = e.getX
      val clickY = e.getY

      val caseNumberX = (clickX - margin) / caseWidth
      val caseNumberY = (clickY - margin) / caseWidth


      val cellX: Int = caseNumberX * caseWidth + margin
      val cellY: Int = caseNumberY * caseWidth + margin - fontSize

      display.setColor(Color.BLUE)
      display.drawFillRect(cellX, cellY, caseWidth, caseWidth)
      resolveMatches()

    }

    override def mouseReleased(e: MouseEvent): Unit = {}

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mouseExited(e: MouseEvent): Unit = {}
  })

}
