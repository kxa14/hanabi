package hanabi

import accessories.Player
import action.Move
import cats.Monad
import cats.effect.Sync
import cats.implicits._
import displayObjects.GameState
import hanabi.Request.RequestMove

final class OnePlayerGameLoop[F[_]: Monad: Sync](
    requestMove: RequestMove[F],
    hintMoveExecutor: HintMoveExecutor[F],
    playMoveExecutor: PlayMoveExecutor[F],
    discardMoveExecutor: DiscardMoveExecutor[F]
) {

  def start(gameState: F[GameState], currentPlayer: Player): F[GameState] = {
    for {
      currentGS <- gameState
      _ <- Sync[F].delay(println(currentGS.display(currentPlayer)))
      finalGS <-
        if (currentGS.life.count == 0) currentGS.pure[F]
        else if (
          currentPlayer.hand.isEmpty && currentGS.hintTokens.count == 0
        ) {
          println(
            s"Skip ${currentPlayer.name} because you've nothing to play on..."
          )
          currentGS.pure[F]
        } else
          requestMove.run(currentPlayer, currentGS.hintTokens).flatMap {
            case Move.Hint =>
              hintMoveExecutor.execute(currentGS, currentPlayer)
            case Move.Play =>
              playMoveExecutor.execute(currentGS, currentPlayer)
            case Move.Discard =>
              discardMoveExecutor.execute(currentGS, currentPlayer)
          }
    } yield finalGS
  }
}
