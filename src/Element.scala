class Element(var value: Int, var x: Int, var y: Int) {

  var isPartOfMatch: Boolean = false
  var countVerticalMoves: Int = 0
  var display: Boolean = true
  var toMove:Boolean = false


  def update(newValue: Int, newX: Int, newY: Int): Unit = {
    value = newValue
    x = newX
    y = newY
  }

  def updateValue(newValue: Int): Unit = {
    value = newValue
  }

  def updateX(newX: Int): Unit = {
    x = newX
  }

}
