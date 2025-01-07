class Element(var value: Image) {

  var isPartOfMatch: Boolean = false
  var countVerticalMoves: Int = 0
  var display: Boolean = true
  var toMove:Boolean = false
  var toGenerate:Boolean = false

  def updateValue(newValue: Image): Unit = {
    value = newValue
  }


  def copy(): Element = {
    val newElement = new Element(value)
    newElement.isPartOfMatch = this.isPartOfMatch
    newElement.countVerticalMoves = this.countVerticalMoves
    newElement.display = this.display
    newElement.toMove = this.toMove
    newElement.toGenerate = this.toGenerate
    newElement
  }





}
