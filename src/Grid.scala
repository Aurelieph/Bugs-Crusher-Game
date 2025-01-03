import hevs.graphics.FunGraphics

import java.awt.Color
import scala.util.Random

class Position(var x: Int, var y: Int) {
  def this() = {
    this(-1, -1)
  }

  def isEmpty(): Boolean = {
    if (this.x == -1 && this.y == -1) {
      true
    }
    else false
  }
}

class Grid(val width: Int, val height: Int, val nbOfElement: Int, val display: FunGraphics) {
  val box: Array[Array[Element]] = Array.ofDim(nbOfElement, nbOfElement)
  val margin: Int = 40
  val fontSize: Int = 14
  val caseWidth: Int = (width - margin * 2) / nbOfElement
  val boxWidth: Int = caseWidth * nbOfElement
  val possibilities: Array[Int] = Array(1, 2, 3, 4, 5)
  var select1: Position = new Position()
  var select2: Position = new Position()


  def initializeElements(): Unit = {
    for (i <- box.indices) {
      for (j <- box(i).indices) {
        box(i)(j) = new Element(randomElement(possibilities))

      }
    }
  }

  def drawElementsTest(): Unit = {
    var iCount = 0
    var jCount = 0
    for (i <- margin to boxWidth by caseWidth) {
      jCount = 0
      for (j <- margin to boxWidth by caseWidth) {
        if (box(iCount)(jCount).toMove) {
          display.drawString(i + 10, j, box(iCount)(jCount).toMove.toString, Color.green, 10)
        }
        if (box(iCount)(jCount).toGenerate) {
          display.drawString(i + 10, j + 10, box(iCount)(jCount).toGenerate.toString, Color.blue, 10)
        }
        if (box(iCount)(jCount).countVerticalMoves > 0) {
          display.drawString(i + 10, j + 20, box(iCount)(jCount).countVerticalMoves.toString, Color.red, 10)
        }

        jCount += 1
      }
      iCount += 1
    }
  }

  def resolveGrid(): Unit = {

    while (identifyMatch()) {
      identifyVerticalMoves()
      explodeElements()
      resolveCascading()
    }

  }

  def identifyMatch(highJack: Boolean = false): Boolean = {
    val impossibleValue = 99
    var lastMatch: Int = impossibleValue
    var matchCount: Int = 0
    var isMatch: Boolean = false

    try {
      //go through vertically
      for (i <- box.indices) {
        for (j <- box(i).indices) {
          if (box(i)(j).value == lastMatch) {
            matchCount += 1
            if (matchCount >= 2) {
              if (highJack) {
                return true
              }
              isMatch = true
              for (k <- 0 to matchCount) {
                box(i)(j - k).isPartOfMatch = true
                box(i)(j - k).display = false
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
    }
    //check horizontally
    try {
      matchCount = 0
      lastMatch = impossibleValue
      for (j <- box.indices) {
        for (i <- box(j).indices) {
          if (box(i)(j).value == lastMatch) {
            matchCount += 1
            if (matchCount >= 2) {
              if (highJack) {
                return true
              }
              isMatch = true
              for (k <- 0 to matchCount) {
                box(i - k)(j).isPartOfMatch = true
                box(i - k)(j).display = false
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
    }
    catch {
      case e: Exception => println("identifyMatch: ", e.printStackTrace())
    }

    isMatch
  }

  // Update the countVerticalMoves value to indicate how many positions each element will drop down after the matches.
  def identifyVerticalMoves(): Unit = {
    try {
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
    catch {
      case e: Exception => println("identifyVerticalMoves: ", e.printStackTrace())
    }
  }

  def resolveCascading(): Unit = {
    var isRunning: Boolean = false
    do {
      Thread.sleep(200)
      display.clear()
      isRunning = cascadingElement()
      drawElements()

    }
    while (isRunning)
  }

  def cascadingElement(): Boolean = {
    var isCascading = false
    //update new positions by counting how many space the element will need to drop
    //Change toMove field to true if the element hasn't been destroyed.
    try {
      for (i <- box.indices) {
        for (j <- box(i).indices) {
          if (box(i)(j).countVerticalMoves > 0) {
            box(i)(j).countVerticalMoves -= 1
            if (!box(i)(j).isPartOfMatch) {
              box(i)(j).toMove = true
            }
            if (box(i)(j).countVerticalMoves > 0) {
              isCascading = true
            }

          }
        }
      }
      //place element at the right position, starting from the bottom
      for (i <- box.indices) {
        for (j: Int <- box(i).indices) {
          if (box(i)(nbOfElement - j - 1).toMove) {

            box(i)(nbOfElement - j - 1).toMove = false
            box(i)(nbOfElement - j) = box(i)(nbOfElement - j - 1).copy()
            box(i)(nbOfElement - j - 1).display = false
            if (nbOfElement - j - 1 == 0) {
              box(i)(nbOfElement - j - 1).toGenerate = true
            }

          }
          if (nbOfElement - j - 1 == 0 && !box(i)(nbOfElement - j - 1).display) {
            box(i)(nbOfElement - j - 1).toGenerate = true
          }
        }
      }

      for (i <- box.indices) {
        for (j: Int <- box(i).indices) {
          if (box(i)(j).toGenerate) {
            val number: Int = randomElement(possibilities)
            box(i)(j).updateValue(number)
            box(i)(j).toGenerate = false
            box(i)(j).isPartOfMatch = false
            box(i)(j).display = true
          }
        }
      }
    }
    catch {
      case e: Exception => println("cascadingElement: ", e.printStackTrace())
    }
    isCascading
  }

  def randomElement(arr: Array[Int]): Int = {
    arr(Random.nextInt(arr.length))
  }

  def explodeElements(): Unit = {
    display.clear()

    var increment = 0
    do {
      display.clear()
      drawElements(animation = true, increment)
      Thread.sleep(50)
      increment += 2
    }
    while (increment < 30)

    clean()
  }

  def clean(): Unit = {
    display.clear()
    drawElements()
  }

  def drawElements(animation: Boolean = false, addSize: Int = 10): Unit = {
    var iCount = 0
    var jCount = 0
    try {
      for (i <- margin to boxWidth by caseWidth) {
        jCount = 0
        for (j <- margin to boxWidth by caseWidth) {
          if (box(iCount)(jCount).display) {
            display.drawString(i + caseWidth / 2 - 3, j + caseWidth / 2 + 3, box(iCount)(jCount).value.toString, new Color(0, 0, 0), fontSize)

          }
          else if (animation && !box(iCount)(jCount).display) {
            display.drawString(i + caseWidth / 2 - 3 - (fontSize + addSize) / 4, j + caseWidth / 2 + 3 + (fontSize + addSize) / 4, box(iCount)(jCount).value.toString, new Color(0, 0, 0), fontSize + addSize)
          }
          jCount += 1
        }
        iCount += 1
      }
    }
    catch {
      case e: Exception => println("drawElements: ", e.printStackTrace())
    }

  }

  def select(x: Int, y: Int): Boolean = {
    var areThereTwoSelections = false
    val cellX: Int = x * caseWidth + margin
    val cellY: Int = y * caseWidth + margin
    // reinitialise if there is a weird combination
    if (select1.x == -1 || select1.y == -1) {
      resetSelection()
    }
    else if (!select2.isEmpty() && (select2.x == -1 || select2.y == -1)) {
      select2 = new Position()
    }

    // check which variable need to be assigned
    if (select1.isEmpty()) {
      select1 = new Position(x, y)
      drawSelection(cellX, cellY)
    }
    else if (!select1.isEmpty() && select2.isEmpty() && isNeighbour(select1, x, y)) {
      select2 = new Position(x, y)
      drawSelection(cellX, cellY)
      areThereTwoSelections = true

    }

    else {
      select1 = new Position()
      select2 = new Position()
      clean()
    }

    areThereTwoSelections
  }

  def isNeighbour(position: Position, x: Int, y: Int): Boolean = {
    !(position.x == x && position.y == y) &&
      (x == position.x && y >= position.y - 1 && y <= position.y + 1 ||
        (x >= position.x - 1 && x <= position.x + 1 && y == position.y))
  }

  def drawSelection(x: Int, y: Int): Unit = {
    display.setColor(Color.BLUE)
    display.drawCircle(x, y, caseWidth)
  }

  def rollBack(): Boolean = {
    switchPosition()
    resetSelection()
    false

  }

  def switchPosition(): Boolean = {
    var isValid: Boolean = false

    val temp: Element = box(select1.x)(select1.y)
    box(select1.x)(select1.y) = box(select2.x)(select2.y).copy()
    box(select2.x)(select2.y) = temp
    clean()
    if (identifyMatch(true)) {
      isValid = true
      resetSelection()
    }
    Thread.sleep(300)
    isValid
  }

  def resetSelection(): Unit = {
    select1 = new Position()
    select2 = new Position()
  }

}
