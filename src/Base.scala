import hevs.graphics.FunGraphics

import java.awt.Color
import java.awt.event.{MouseEvent, MouseListener}
import scala.util.Random

object Base extends App{
  val width:Int = 600
  val height:Int = 800
  val nbOfElement: Int = 9
  val display: FunGraphics = new FunGraphics(width, height, "Our Super Game")

  val box: Array[Array[Int]] = Array.ofDim(nbOfElement,nbOfElement)

  val margin:Int = 40
  val caseWidth: Int = (width-margin*2)/nbOfElement
  val boxWidth:Int = caseWidth*nbOfElement
  val possibilities:Array[Int] = Array(1,2,3,4,5)
  val fontSize:Int = 8
  def initializeElements(arr:Array[Int]):Unit= {
    for (i <- box.indices) {
      for (j <- box(i).indices) {
        box(i)(j) =arr(Random.nextInt(arr.length))
      }
    }
  }
  def displayFillBox(arr:Array[Array[Int]]):Unit = {
    var iCount = 0
    var jCount = 0
    for (i<- margin to boxWidth by caseWidth){
      jCount = 0
      for(j<- margin to boxWidth by caseWidth) {
        display.drawString(i,j,arr(iCount)(jCount).toString,new Color(0,0,0),fontSize)
        jCount +=1
      }
      iCount+=1
    }
  }





  initializeElements(possibilities)
  displayFillBox(box)

  display.addMouseListener(new MouseListener {
    override def mouseClicked(e: MouseEvent): Unit = {
      val clickX = e.getX
      val clickY = e.getY

      val cellX = (clickX-margin) / caseWidth
      val cellY = (clickY-margin) / caseWidth


      display.setColor(Color.BLUE)
      display.drawFillRect(cellX * caseWidth+margin-fontSize, cellY * caseWidth+margin-fontSize, caseWidth, caseWidth)
      println(clickX,clickY)
      println(cellX,cellY)
    }

    override def mousePressed(e: MouseEvent): Unit = {}

    override def mouseReleased(e: MouseEvent): Unit = {}

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mouseExited(e: MouseEvent): Unit = {}
  })


}
