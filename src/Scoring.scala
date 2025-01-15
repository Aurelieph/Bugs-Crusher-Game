import hevs.graphics.utils.GraphicsBitmap

// Class to store the score of the current level
class Scoring(var level: Int) {
  var score: Int = 0
  var goal: Int = 1000
  var movesLeft = 10 - level + 1
  val lastLevel = 5

  def decreaseMove(): Unit = {
    movesLeft -= 1
  }

  def increaseScore(addScore: Int): Unit = {
    score += addScore
  }

  def goalReached(): Boolean = {
    score >= goal
  }

  def isVictory(): Boolean = {
    level > lastLevel || (level == lastLevel && isLevelFinished())
  }

  // Show different images based on the state of the game
  def endMessage(): GraphicsBitmap = {
    val levelup = new GraphicsBitmap("/res/level.png")
    val gameover = new GraphicsBitmap("/res/gameover.png")
    val victory = new GraphicsBitmap("/res/win.png")

    if (isVictory()) {
      return victory
    }
    if (isLevelFinished && !goalReached()) {
      return gameover
    }
    levelup
  }

  def isLevelFinished(): Boolean = {
    movesLeft <= 0

  }

  def endGame(): Boolean = {
    isVictory() || (isLevelFinished() && !goalReached())
  }
}
