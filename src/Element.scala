class Element(var value: Int, var x: Int, var y: Int) {

  var isPartOfMatch: Boolean = false
  var countVerticalMoves: Int = 0
  var display: Boolean = true
  var toMove:Boolean = false
  var toGenerate:Boolean = false


  def update(newValue: Int, newX: Int, newY: Int): Unit = {
    value = newValue
    //x = newX
    //y = newY
  }

  def updateValue(newValue: Int): Unit = {
    value = newValue
  }


  def copy(): Element = {
    val newElement = new Element(value, x, y)
    newElement.isPartOfMatch = this.isPartOfMatch
    newElement.countVerticalMoves = this.countVerticalMoves
    newElement.display = this.display
    newElement.toMove = this.toMove
    newElement.toGenerate = this.toGenerate
    newElement
  }





}
