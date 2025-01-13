import hevs.graphics.utils.GraphicsBitmap

class Scoring(var level:Int) {
  var score:Int = 0
  var goal:Int = 300
  var movesLeft = 11 - level

  def decreaseMove(): Unit = {
    movesLeft -= 1
  }

  def increaseScore(addScore: Int): Unit = {
    score += addScore
  }

  def goalReached(): Boolean = {
    score >= goal
  }

  def endMessage(): GraphicsBitmap = {
    val levelup = new GraphicsBitmap("/res/level.png")
    val gameover = new GraphicsBitmap("/res/gameover.png")
    val end = new GraphicsBitmap("/res/win2.png")

    if (level > 5) {
      return end
    }
    if (movesLeft <= 0 && !goalReached()){
      return gameover
    }
    levelup
  }

  def isLevelFinished(): Boolean = {
    if (movesLeft <= 0){
      return true
    }
    false
  }

  def endGame(): Boolean = {
  level > 5 || (movesLeft <= 0 && !goalReached())
  }
}
