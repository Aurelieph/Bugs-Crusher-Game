import hevs.graphics.FunGraphics
import hevs.graphics.utils.GraphicsBitmap

import java.awt.Color
import javax.swing.SwingConstants
import scala.util.Random

class Position(var x: Int, var y: Int) {
  def this() = {
    this(-1, -1)
  }

  def isEmpty(): Boolean = {
    this.x == -1 && this.y == -1
  }
}

// Class that manages the grid and the main features of the game
class Grid(val width: Int, val height: Int, val nbOfElement: Int, val display: FunGraphics) {
  val box: Array[Array[Element]] = Array.ofDim(nbOfElement, nbOfElement)
  val leftMargin: Int = 40
  val topMargin: Int = 80
  val caseWidth: Int = (width - leftMargin * 2) / nbOfElement
  val boxWidth: Int = caseWidth * nbOfElement
  val possibilities: Array[String] = Array("/original/bug_big_eyes.png", "/original/bug_big_nose_blue.png", "/original/bug_eyes.png", "/original/bug_green.png", "/original/bug_smile.png")
  val bonus: String = "/oiseau angry_small.png"
  var select1: Position = new Position()
  var select2: Position = new Position()
  var currentLevel: Int = 1
  var level = new Scoring(currentLevel)
  var totalScore: Int = 0
  val fontSize: Int = 24
  val buttonWidth = 150
  val buttonHeight = 60
  val buttonX = display.getFrameWidth() / 2 - buttonWidth / 2
  val buttonY = topMargin + boxWidth + topMargin / 2
  var selectable: Boolean = false

  // Initiate the game
  // Params:
  //  restart – Indicate if the game restart after one has been already played
  //  completely - Indicate if we restart the game from scratch
  def start(restart: Boolean = false, completely: Boolean = false): Unit = {
    selectable = true
    display.frontBuffer.synchronized {

      if (restart) {
        display.clear()
      }
      if (completely) {
        totalScore = 0
        currentLevel = 1
        resetSelection()
        level = new Scoring(currentLevel)
      }
      initializeElements()
      resolveGrid(pregame = true)
      drawElements()
    }
  }

  // Select a random element from the given array
  def randomElement(arr: Array[String]): String = {
    arr(Random.nextInt(arr.length))
  }

  //Initiate the grid with new randomized element in each position
  def initializeElements(): Unit = {
    for (i <- box.indices) {
      for (j <- box(i).indices) {
        box(i)(j) = new Element(randomElement(possibilities))

      }
    }
  }

  // Guaranty that all possible matches are resolved
  def resolveGrid(pregame: Boolean = false): Unit = {
    // Resolve matches until no more are found
    def resolveMatches(): Unit = {
      while (identifyMatch(highJack = false, pregame)) {
        identifyVerticalMoves()
        if (!pregame) {
          explodeElements()
        }
        resolveCascading(pregame)
        displayScoring()
      }
    }

    resolveMatches()
    nextLevel()
    while (!isThereAPossibleMove()) {
      shuffle()
      resolveMatches()
    }
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

  // Manage each element that needs to move down
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
            box(i)(nbOfElement - j - 1).bonus = false
            if (nbOfElement - j - 1 == 0) {
              box(i)(nbOfElement - j - 1).toGenerate = true
            }

          }
          if (nbOfElement - j - 1 == 0 && !box(i)(nbOfElement - j - 1).display && !box(i)(nbOfElement - j - 1).bonus) {
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
          else if (box(i)(j).bonus) {
            box(i)(j).updateValue(bonus)
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

  // The logic to keep moving down the elements if they need to fall down
  def resolveCascading(pregame: Boolean = false): Unit = {
    var isRunning: Boolean = false
    do {
      if (!pregame) {
        Thread.sleep(100)

      }
      isRunning = cascadingElement()
      if (!pregame) {
        drawElements()
      }
    }
    while (isRunning)
  }

  // Animation to show which elements will disappear
  def explodeElements(): Unit = {

    var increment = 0
    do {
      drawElements(animation = true, increment)
      Thread.sleep(50)
      increment += 1
    }
    while (increment < 15)
  }

  // Verify if the player can make a possible move
  def isThereAPossibleMove(): Boolean = {
    for (i <- box.indices) {
      for (j <- box(i).indices) {
        if (box(i)(j).bonus) {
          return true
        }
      }
    }

    for (i <- 0 until box.length - 1) {
      for (j <- 0 until box.length - 1) {
        val temp: Element = box(i)(j)
        box(i)(j) = box(i)(j + 1).copy()
        box(i)(j + 1) = temp

        val isPossible = identifyMatch(highJack = true)

        box(i)(j + 1) = box(i)(j).copy()
        box(i)(j) = temp

        if (isPossible) {
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

  // Manage the selection of the element
  // return true if 2 valid elements are selected
  def select(x: Int, y: Int): Boolean = {
    var areThereTwoSelections = false
    val cellX: Int = x * caseWidth + leftMargin
    val cellY: Int = y * caseWidth + topMargin
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

  // Check if a element is a horizontal or vertical neighbour (not diagonally)
  def isNeighbour(position: Position, x: Int, y: Int): Boolean = {
    !(position.x == x && position.y == y) &&
      (x == position.x && y >= position.y - 1 && y <= position.y + 1 ||
        (x >= position.x - 1 && x <= position.x + 1 && y == position.y))
  }

  // Circle the elements we select
  def drawSelection(x: Int, y: Int, color: Color = Color.blue): Unit = {
    display.setColor(color)
    display.drawCircle(x, y, caseWidth)
    display.drawCircle(x - 1, y - 1, caseWidth + 2)
    display.drawCircle(x - 2, y - 2, caseWidth + 4)
  }

  // Reverse the elements position
  def switchBack(): Unit = {
    switchPosition()
    resetSelection()
  }

  // Switch the 2 selected elements
  // return true if the switch is valid (will create a match)
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
    Thread.sleep(200)
    isValid
  }

  // Verify if there are possible matches and update element variable if this is the case.
  //  HighJack argument is used to only know if there is at least one match. To save perf.
  //  Pregame argument is to indicate if we want to resolve everything before a game start to have a clean game
  //  with no already created match
  def identifyMatch(highJack: Boolean = false, pregame: Boolean = false): Boolean = {
    val impossibleValue = "99"
    var lastMatch: String = impossibleValue
    var matchCount: Int = 0
    var isMatch: Boolean = false

    def updateMatches(index1: Int, index2: Int, k: Int) = {
      box(index1)(index2).isPartOfMatch = true
      box(index1)(index2).display = false
      if (!pregame) {
        level.increaseScore(10)
        //create bonus
        if (matchCount >= 3 && k == matchCount) {
          box(index1)(index2).bonus = true
          box(index1)(index2).isPartOfMatch = false
        }
      }
    }

    try {
      //check if the bonus is activated
      if (!select1.isEmpty() && !select2.isEmpty()) {
        if (box(select1.x)(select1.y).bonus) {
          box(select1.x)(select1.y).bonusIsActivated = true
          if (highJack) {
            return true
          }
        }
        if (box(select2.x)(select2.y).bonus) {
          box(select2.x)(select2.y).bonusIsActivated = true
          if (highJack) {
            return true
          }
        }
      }

      for (i <- box.indices) {
        for (j <- box(i).indices) {
          if (box(i)(j).bonusIsActivated) {

            box(i)(j).bonusIsActivated = false
            for (k <- box.indices) {
              box(k)(j).isPartOfMatch = true
              box(k)(j).display = false
              box(i)(j).bonus = false
              level.increaseScore(10)
            }
            for (k <- box(i).indices) {
              box(i)(k).isPartOfMatch = true
              box(i)(k).display = false
              box(i)(k).bonus = false
              level.increaseScore(10)

            }
            isMatch = true
          }
        }
      }
    }

    try {
      //go through vertically
      for (i <- box.indices) {
        for (j <- box(i).indices) {
          if (box(i)(j).value == lastMatch && !box(i)(j).bonus) {
            matchCount += 1
            if (matchCount >= 2) {
              if (highJack) {
                return true
              }
              isMatch = true
              if (j == box(i).length - 1) {
                for (k <- 0 to matchCount) {
                  updateMatches(i, j - k, k)
                }
              }
            }
          }
          else {
            if (matchCount >= 2) {
              for (k <- 0 to matchCount) {
                updateMatches(i, j - k - 1, k)
              }
            }
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
          if (box(i)(j).value == lastMatch && !box(i)(j).bonus) {
            matchCount += 1
            if (matchCount >= 2) {
              if (highJack) {
                return true
              }
              isMatch = true
              if (i == box(j).length - 1) {

                for (k <- 0 to matchCount) {
                  updateMatches(i - k, j, k)
                }
              }

            }

          }
          else {
            if (matchCount >= 2) {
              for (k <- 0 to matchCount) {
                updateMatches(i - k - 1, j, k)
              }
            }
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

  // clean the grid and rewrite the elements
  def clean(): Unit = {
    display.frontBuffer.synchronized {
      display.clear()
      drawElements()
    }
  }


  // Draw elements depending on their state
  //  animation argument allow to change position to animate the elements (dancing elements)
  //  addSize argument is the value we want to increase the size of each element during animation
  def drawElements(animation: Boolean = false, addSize: Int = 10): Unit = {
    var iCount = 0
    var jCount = 0
    try {
      display.frontBuffer.synchronized {
        drawUI()
        val darkGreen = new Color(116, 132, 4)
        val imageOffset = 2

        for (i <- leftMargin until boxWidth + leftMargin by caseWidth) {
          jCount = 0
          for (j <- topMargin until boxWidth + topMargin by caseWidth) {
            if (box(iCount)(jCount).display) {
              display.setColor(darkGreen)
              display.drawRect(i, j, caseWidth, caseWidth)
              display.drawTransformedPicture(i + caseWidth / 2 + imageOffset, j + caseWidth / 2, 0, 0.2, box(iCount)(jCount).bitmap)
            }
            else if (animation && (!box(iCount)(jCount).display) && addSize % 2 == 0) {
              display.setColor(darkGreen)
              display.drawRect(i, j, caseWidth, caseWidth)
              display.drawTransformedPicture(i + caseWidth / 2 + imageOffset, j + caseWidth / 2, 0.2, 0.2, box(iCount)(jCount).bitmap)
            }
            else if (animation && !box(iCount)(jCount).display) {
              display.setColor(darkGreen)
              display.drawRect(i, j, caseWidth, caseWidth)
              display.drawTransformedPicture(i + caseWidth / 2 + imageOffset, j + caseWidth / 2, -0.2, 0.2, box(iCount)(jCount).bitmap)
            }
            else {
              display.setColor(darkGreen)
              display.drawRect(i, j, caseWidth, caseWidth)
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

  def drawFancyString(x: Int, y: Int, str: String, size: Int = fontSize): Unit = {
    display.drawFancyString(
      posX = x,
      posY = y,
      str = str,
      fontFamily = "Helvetica",
      fontSize = size,
      halign = SwingConstants.CENTER,
      valign = SwingConstants.CENTER)
  }

  def displayScoring(): Unit = {
    val labelHeight = 40
    val labelWidth: Int = 140
    val pink = new Color(254, 224, 246)
    display.setColor(pink)

    //display level
    display.drawFilledOval(display.getFrameWidth() / 2 - labelWidth / 2, display.getFrameHeight() - leftMargin - labelHeight / 2, labelWidth, labelHeight)
    drawFancyString(display.getFrameWidth() / 2, display.getFrameHeight() - leftMargin - fontSize / 4, s"Level: ${level.level}")

    //display target and score
    display.drawFilledOval(display.getFrameWidth() - labelWidth - 2 * leftMargin - 20, topMargin + boxWidth + 20, labelWidth, leftMargin)
    drawFancyString(display.getFrameWidth() - labelWidth / 2 - 2 * leftMargin - 20, topMargin + boxWidth + 2 * labelHeight, s"Reach:${level.goal}", 18)
    drawFancyString(display.getFrameWidth() - labelWidth / 2 - 2 * leftMargin - 20, topMargin + boxWidth + 20 + labelHeight / 2 - fontSize / 4, s"${level.score}")

    //display moves
    display.drawFilledOval(2 * leftMargin + 20, topMargin + boxWidth + 20, labelWidth, leftMargin)
    drawFancyString(2 * leftMargin + labelWidth / 2 + 20, topMargin + boxWidth + 2 * labelHeight, "Moves left:", 18)
    drawFancyString(2 * leftMargin + labelWidth / 2 + 20, topMargin + boxWidth + 20 + labelHeight / 2 - fontSize / 4, s"${level.movesLeft}")


  }

  def drawUI(): Unit = {
    val background = new GraphicsBitmap("/res/grass.jpg")
    val title = new GraphicsBitmap("/BugsCrusherTitle.png")
    val mascotGrace = new GraphicsBitmap("/res/mascot1.png")
    val mascotAurelie = new GraphicsBitmap("/res/mascot2.png")
    val lightGreen = new Color(189, 209, 12)
    display.drawTransformedPicture(0, 0, 0, 1, background)
    display.setColor(lightGreen)
    display.drawFillRect(leftMargin, topMargin, boxWidth, boxWidth)

    //display title
    display.drawTransformedPicture(display.getFrameWidth() / 2, topMargin / 2, 0, 0.3, title)
    //display mascots
    display.drawTransformedPicture(leftMargin, boxWidth + 2 * topMargin, 0, 0.5, mascotGrace)
    display.drawTransformedPicture(boxWidth + leftMargin, boxWidth + 2 * topMargin, 0, 0.5, mascotAurelie)

    displayScoring()
  }

  def resetSelection(): Unit = {
    select1 = new Position()
    select2 = new Position()
  }

  // Mix all the elements in different positions (usefull if there is no available possible moves)
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

  // Manage the end of a level and start a new level/game or end the game
  def nextLevel(): Unit = {
    if (level.isLevelFinished()) {
      totalScore += level.score

      //next level message
      if (level.goalReached() && !level.isVictory()) {
        currentLevel += 1
        level = new Scoring(currentLevel)
        display.drawTransformedPicture(display.getFrameWidth() / 2, display.getFrameWidth() / 2 + topMargin / 2, 0, 0.5, level.endMessage())
        Thread.sleep(2000)
      }
      //beat the game message
      else if (level.isVictory()) {
        selectable = false
        display.drawTransformedPicture(display.getFrameWidth() / 2, boxWidth / 2, 0, 0.75, level.endMessage())
        drawFancyString(display.getFrameWidth() / 2, boxWidth / 2 + 70, totalScore.toString)
        Thread.sleep(2000)
      }
      //lost the game message
      else {
        selectable = false
        display.drawTransformedPicture(display.getFrameWidth() / 2, display.getFrameWidth() / 2 + topMargin / 2, 0, 0.75, level.endMessage())
        Thread.sleep(2000)

      }
      if (!level.endGame()) {
        start(restart = true)
      }
      else {
        display.drawFillRect(buttonX, buttonY, buttonWidth, buttonHeight)
        drawFancyString(display.getFrameWidth() / 2, topMargin + boxWidth + topMargin / 2 + buttonHeight / 2 - fontSize / 4, "RESTART")

      }
    }
  }

  // Manage the restart button at the end of the game
  def clickButton(x: Int, y: Int): Boolean = {
    display.setColor(Color.GRAY)
    display.drawFillRect(buttonX, buttonY, buttonWidth, buttonHeight)
    drawFancyString(display.getFrameWidth() / 2, topMargin + boxWidth + topMargin / 2 + buttonHeight / 2 - fontSize / 4, "RESTART")
    Thread.sleep(200)
    display.setColor(Color.WHITE)
    display.drawFillRect(buttonX, buttonY, buttonWidth, buttonHeight)
    drawFancyString(display.getFrameWidth() / 2, topMargin + boxWidth + topMargin / 2 + buttonHeight / 2 - fontSize / 4, "RESTART")

    x > buttonX && x < buttonX + buttonWidth && y > buttonY && y < buttonY + buttonHeight
  }
}
