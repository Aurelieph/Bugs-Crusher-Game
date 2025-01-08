class Scoring(var level:Int=1) {
  var score:Int = 0
  var goal:Int = 100
  var movesLeft = 10

  def resetScore(): Unit = {
    score = 0
  }

  def increaseGoal(newLevel: Int): Unit = {
    goal = newLevel * 100
  }

  def decreaseMove(newLevel: Int): Unit = {
    movesLeft -= newLevel
  }
}
