import hevs.graphics.FunGraphics

import java.awt.event.{MouseEvent, MouseListener}

object Game extends App {
  val width: Int = 600
  val height: Int = 800
  val nbOfElement: Int = 8
  val display: FunGraphics = new FunGraphics(width, height, 450, 0, "Bugs Crusher", true)
  var game: Grid = new Grid(width, height, nbOfElement, display)

  game.start()

  display.addMouseListener(new MouseListener {
    override def mouseClicked(e: MouseEvent): Unit = {}

    override def mousePressed(e: MouseEvent): Unit = {
      // Convert the position of the pointer to the grid position index
      val clickX = e.getX
      val clickY = e.getY
      val caseNumberX = (clickX - game.leftMargin) / game.caseWidth
      val caseNumberY = (clickY - game.topMargin) / game.caseWidth

      // Activate the resolution of the grid if the 2 selected numbers are authorized
      // if, not rollback the positions switch
      if (game.selectable && game.select(caseNumberX, caseNumberY)) {
        if (game.switchPosition()) {
          game.resolveGrid()
        }
        else {
          game.switchBack()
        }
      }
      // If the game si finished, capture if the player click on the restart button
      else if (!game.selectable && game.clickButton(clickX, clickY)) {
        game.start(restart = true, completely = true)
      }
    }

    override def mouseReleased(e: MouseEvent): Unit = {}

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mouseExited(e: MouseEvent): Unit = {}
  })

}
