import hevs.graphics.utils.GraphicsBitmap

class Element(var value: String) {

  var isPartOfMatch: Boolean = false
  var countVerticalMoves: Int = 0
  var display: Boolean = true
  var toMove:Boolean = false
  var toGenerate:Boolean = false
  var bitmap = new GraphicsBitmap(value)
  var bonus:Boolean = false
  var bonusIsActivated:Boolean = false

  def updateValue(newValue: String): Unit = {
    value = newValue
    bitmap = new GraphicsBitmap(value)
  }

  def copy(): Element = {
    val newElement = new Element(value)
    newElement.isPartOfMatch = this.isPartOfMatch
    newElement.countVerticalMoves = this.countVerticalMoves
    newElement.display = this.display
    newElement.toMove = this.toMove
    newElement.toGenerate = this.toGenerate
    newElement.bonus = this.bonus
    newElement.bonusIsActivated = this.bonusIsActivated
    newElement
  }

}
