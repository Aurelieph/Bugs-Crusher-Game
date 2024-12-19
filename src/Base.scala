import hevs.graphics.FunGraphics

object Base extends App{
  val display: FunGraphics = new FunGraphics(600, 800, "Our Super Game")

  val box: Array[Array[Int]] = Array.ofDim(9,9)
  // val color: List[Int]

  for (i <- box.indices){
    for (j <- box(i).indices){
      box(i)(j) = (Math.random()*5).toInt
    }
  }


}
