import hevs.graphics.FunGraphics
import hevs.graphics.utils.GraphicsBitmap

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
  val caseWidth: Int = (width - margin * 2) / nbOfElement
  val boxWidth: Int = caseWidth * nbOfElement
  val possibilities: Array[String] = Array("/res/blue.png", "/res/green.png", "/res/purple.png", "/res/red.png", "/res/yellow.png")
  var select1: Position = new Position()
  var select2: Position = new Position()
  var currentLevel: Int = 1
  var level = new Scoring(currentLevel) // 1 needs to be an incremented var


  def start(restart: Boolean = false) = {
    if (restart) {
      display.clear()
    }
    initializeElements()
    drawElements()
    resolveGrid()
    isThereAPossibleMove()
  }

  def randomElement(arr: Array[String]): String = {
    arr(Random.nextInt(arr.length))
  }

  def initializeElements(): Unit = {
    for (i <- box.indices) {
      for (j <- box(i).indices) {
        box(i)(j) = new Element(randomElement(possibilities))

      }
    }
  }

  def resolveGrid(): Unit = {
    // Resolve matches until no more are found
    def resolveMatches(): Unit = {
      while (identifyMatch()) {
        identifyVerticalMoves()
        explodeElements()
        resolveCascading()
        displayScoring()
      }
    }

    // Resolution logic
    resolveMatches()
    nextLevel()
    while (!isThereAPossibleMove()) {
      shuffle()
      resolveMatches()
    }
  }

  def drawElements(animation: Boolean = false, addSize: Int = 10): Unit = {
    displayScoring()
    var iCount = 0
    var jCount = 0
    try {
      display.frontBuffer.synchronized {
        for (i <- margin to boxWidth by caseWidth) {
          jCount = 0
          for (j <- margin to boxWidth by caseWidth) {
            if (box(iCount)(jCount).display) {
              display.drawPicture(i + caseWidth / 2, j + caseWidth / 2, box(iCount)(jCount).bitmap)
            }
            else if (animation && !box(iCount)(jCount).display && addSize % 2 == 0) {
              display.drawPicture(i + caseWidth / 2, j + caseWidth / 2, box(iCount)(jCount).bitmap)
            }
            else {
              display.drawPicture(i + caseWidth / 2, j + caseWidth / 2, new GraphicsBitmap("/res/white.png"))
            }
            jCount += 1
          }
          iCount += 1
        }
      }
    }
    catch {
      case e: Exception => println("drawElements: ", e.printStackTrace())
    }

  }

  //highJack argument is used to only know if there is at least one match. To save perf.
  def identifyMatch(highJack: Boolean = false): Boolean = {
    val impossibleValue = "99"
    var lastMatch: String = impossibleValue
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
                level.increaseScore(10)
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
                level.increaseScore(10)
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
            // check remove condition
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
            val file: String = randomElement(possibilities)
            box(i)(j).updateValue(file)
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

  def resolveCascading(): Unit = {
    var isRunning: Boolean = false
    do {
      Thread.sleep(150)
      isRunning = cascadingElement()
      drawElements()

    }
    while (isRunning)
  }

  def explodeElements(): Unit = {
    //display.clear()

    var increment = 0
    do {
      //display.clear()
      drawElements(animation = true, increment)
      Thread.sleep(100)
      increment += 1
    }
    while (increment < 15)

    //clean()
  }

  def isThereAPossibleMove(): Boolean = {

    for (i <- 0 until box.length - 1) {
      for (j <- 0 until box.length - 1) {
        val temp: Element = box(i)(j)
        box(i)(j) = box(i)(j + 1).copy()
        box(i)(j + 1) = temp

        val isPossible = identifyMatch(highJack = true)

        box(i)(j + 1) = box(i)(j).copy()
        box(i)(j) = temp

        if (isPossible) {
          println(i, j)
          return true
        }
      }
    }
    //show a "no move" message in the middle of the Grid if there is no possible move
    display.setColor(Color.WHITE)
    display.drawFillRect(display.getFrameWidth() / 2 - 52, display.getFrameWidth() / 2 - 20, 100, 40)
    display.setColor(Color.BLACK)
    display.drawRect(display.getFrameWidth() / 2 - 52, display.getFrameWidth() / 2 - 20, 100, 40)
    display.drawString(display.getFrameWidth() / 2 - 28, display.getFrameWidth() / 2 + 4, "NO MOVE", Color.BLACK, 12)
    Thread.sleep(2000)

    false
  }

  def isNeighbour(position: Position, x: Int, y: Int): Boolean = {
    !(position.x == x && position.y == y) &&
      (x == position.x && y >= position.y - 1 && y <= position.y + 1 ||
        (x >= position.x - 1 && x <= position.x + 1 && y == position.y))
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

    if (select1.isEmpty() && x < nbOfElement && y < nbOfElement) {
      select1 = new Position(x, y)
      drawSelection(cellX, cellY)
    }
    else if (!select1.isEmpty() && select2.isEmpty() && isNeighbour(select1, x, y) && x < nbOfElement && y < nbOfElement) {
      select2 = new Position(x, y)
      areThereTwoSelections = true

    }

    else {
      select1 = new Position()
      select2 = new Position()
      clean()
    }

    areThereTwoSelections
  }

  def resetSelection(): Unit = {
    select1 = new Position()
    select2 = new Position()
  }

  def drawSelection(x: Int, y: Int, color: Color = Color.blue): Unit = {
    display.setColor(color)
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
    if (identifyMatch(highJack = true)) {
      isValid = true
      level.decreaseMove()
      resetSelection()
    }
    Thread.sleep(300)
    isValid
  }

  def clean(): Unit = {
    display.clear()
    drawElements()
  }

  def shuffle(): Unit = {

    val tempArray: Array[Array[Element]] = Array.ofDim(nbOfElement, nbOfElement)

    for (i <- box.indices) {
      box(i) = Random.shuffle(box(i).toSeq).toArray
    }

    for (i <- box.indices) {
      for (j <- box(i).indices) {
        tempArray(j)(i) = box(i)(j).copy()
      }
    }
    for (i <- box.indices) {
      box(i) = Random.shuffle(tempArray(i).toSeq).toArray
    }

    for (i <- box.indices) {
      for (j <- box(i).indices) {
        box(i)(j) = tempArray(i)(j)
      }
    }

    clean()
  }

  def displayScoring(): Unit = {
    val height: Int = 40
    val width: Int = 140
    display.setColor(Color.WHITE)
    display.drawFillRect(display.getFrameWidth() / 2 - width / 2, display.getFrameHeight() - margin - height, width, height)
    display.setColor(Color.BLACK)
    display.drawRect(display.getFrameWidth() / 2 - width / 2, display.getFrameHeight() - margin - height, width, height)
    display.drawString(display.getFrameWidth() / 2 - width / 2, display.getFrameHeight() - margin - 50, s"Target: ${level.goal}", Color.BLACK, 22)
    display.drawString(display.getFrameWidth() / 2 - width / 2, display.getFrameHeight() - margin, s"${level.score}", Color.BLACK, 22)
    display.setColor(Color.WHITE)
    display.drawFillRect(margin, display.getFrameHeight() - margin - height, width, height)
    display.drawString(margin, display.getFrameHeight() - margin, s"Moves left: ${level.movesLeft}", Color.BLACK, 22)
    display.drawString(10, 20, s"Level: ${level.level}", Color.BLACK, 22)
  }

  def nextLevel(): Unit = {
    if (level.isLevelFinished()) {
      if (level.goalReached()) {
        currentLevel += 1
        level = new Scoring(currentLevel)
        display.setColor(Color.WHITE)
        display.drawFillRect(display.getFrameWidth() / 2 - 52, display.getFrameWidth() / 2 - 20, 100, 40)
        display.setColor(Color.BLACK)
        display.drawRect(display.getFrameWidth() / 2 - 52, display.getFrameWidth() / 2 - 20, 100, 40)
        display.drawString(display.getFrameWidth() / 2 - 28, display.getFrameWidth() / 2 + 4, level.endMessage(), Color.BLACK, 12)
        Thread.sleep(2000)
      }
      else {
        display.setColor(Color.WHITE)
        display.drawFillRect(display.getFrameWidth() / 2 - 52, display.getFrameWidth() / 2 - 20, 100, 40)
        display.setColor(Color.BLACK)
        display.drawRect(display.getFrameWidth() / 2 - 52, display.getFrameWidth() / 2 - 20, 100, 40)
        display.drawString(display.getFrameWidth() / 2 - 28, display.getFrameWidth() / 2 + 4, level.endMessage(), Color.BLACK, 12)
        Thread.sleep(2000)
      }
      if (!level.endGame()) {
        start(true)
      }
    }
  }


}
