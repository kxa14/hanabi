import displayObjects.GameState

import scala.annotation.tailrec

final class Game {
  lazy private val setupGame: GameState = GameState()

  @tailrec
  private def repeatCycle(gs: GameState): GameState =
    if (gs.life.count == 0) {
      println("Game Over! Hope you all had fun :)")
      gs
    } else {
      repeatCycle(gs.cycle)
    }

  def start: GameState = repeatCycle(setupGame)
}
