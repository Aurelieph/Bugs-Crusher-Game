class Scoring(var level:Int) {
  var score:Int = 0
  var goal:Int = 10000
  var movesLeft = 3 - level

//  def resetScore(): Unit = {
//    score = 0
//  }
//  def increaseGoal(newLevel: Int): Unit = {
//    goal = newLevel * 100
//  }

  def decreaseMove(): Unit = {
    movesLeft -= 1
  }

  def increaseScore(addScore: Int): Unit = {
    score += addScore
  }

  def goalReached(): Boolean = {
    score >= goal
  }

  def endMessage(): String = {
    if (level == 5) {
      return "Congratulation, you beat the game :D"
    }
    if (movesLeft <= 0 && !goalReached()){
      return "Too bad, you lost :("
    }
    "New Level!"
  }

  def isLevelFinished(): Boolean = {
    if (movesLeft <= 0){
      return true
    }
    false
  }

  def endGame(): Boolean = {
  level == 5 || (movesLeft <= 0 && !goalReached())
  }
}
