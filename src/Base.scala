import hevs.graphics.FunGraphics

object Base extends App {
  val width: Int = 600
  val height: Int = 800
  val nbOfElement: Int = 9
  val display: FunGraphics = new FunGraphics(width, height,0,0, "Our Super Game",true)


  var myGrid: Grid = new Grid(width, height, nbOfElement, display)



}
