import hevs.graphics.FunGraphics

import java.awt.event.{MouseEvent, MouseListener}

object Game extends App {
  val width: Int = 600
  val height: Int = 800
  val nbOfElement: Int = 9
  val display: FunGraphics = new FunGraphics(width, height, 0, 0, "Bugs crasher", true)
  var game: Grid = new Grid(width, height, nbOfElement, display)

  game.start(pregame = true)



  display.addMouseListener(new MouseListener {
    override def mouseClicked(e: MouseEvent): Unit = {
    }

    override def mousePressed(e: MouseEvent): Unit = {
      val clickX = e.getX
      val clickY = e.getY
      val caseNumberX = (clickX - game.margin) / game.caseWidth
      val caseNumberY = (clickY - game.margin) / game.caseWidth

      if (game.select(caseNumberX, caseNumberY)) {
        if (game.switchPosition()) {
          game.resolveGrid()
        }
        else {
          game.rollBack()
        }
      }
    }

    override def mouseReleased(e: MouseEvent): Unit = {}

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mouseExited(e: MouseEvent): Unit = {}
  })

}
