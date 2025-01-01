import hevs.graphics.FunGraphics

import java.awt.Color
import java.awt.event.{MouseEvent, MouseListener}

object Base extends App {
  val width: Int = 600
  val height: Int = 800
  val nbOfElement: Int = 9
  val display: FunGraphics = new FunGraphics(width, height, 0, 0, "Bugs crasher", true)
  var myGrid: Grid = new Grid(width, height, nbOfElement, display)

  myGrid.initializeElements()
  myGrid.resolveGrid()

  display.addMouseListener(new MouseListener {
    override def mouseClicked(e: MouseEvent): Unit = {
    }

    override def mousePressed(e: MouseEvent): Unit = {
      val clickX = e.getX
      val clickY = e.getY
      val caseNumberX = (clickX - myGrid.margin) / myGrid.caseWidth
      val caseNumberY = (clickY - myGrid.margin) / myGrid.caseWidth
      val cellX: Int = caseNumberX * myGrid.caseWidth + myGrid.margin
      val cellY: Int = caseNumberY * myGrid.caseWidth + myGrid.margin

      if(myGrid.select(caseNumberX,caseNumberY)){
        myGrid.drawSelection(cellX, cellY)
      }
      else {
        myGrid.clean()
      }
    }

    override def mouseReleased(e: MouseEvent): Unit = {}

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mouseExited(e: MouseEvent): Unit = {}
  })

}
