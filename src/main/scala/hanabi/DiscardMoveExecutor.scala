package hanabi

import accessories.{Card, Player}
import cats.Applicative
import cats.implicits._
import characteristics.CardPosition
import displayObjects.GameState
import hanabi.Request.RequestSingleCardPosition
import monocle.macros.syntax.lens._
import util.GameUtils

sealed trait DiscardMoveExecutor[F[_]] {
  def execute(
      gameState: GameState,
      player: Player
  ): F[GameState]
}

object DiscardMoveExecutor {
  final class LivePlayersDiscardMoveExecutor[F[_]: Applicative](
      requestSingleCardPosition: RequestSingleCardPosition[F]
  ) extends DiscardMoveExecutor[F] {
    override def execute(
        gameState: GameState,
        player: Player
    ): F[GameState] = {
      if (player.hand.isEmpty) gameState.pure[F]
      else
        requestSingleCardPosition
          .run(player)
          .map(cardIndex => {
//            val cardIndex = cp - 1
            val selectedCard: Card = player.hand(cardIndex)
            println(s"""
                       |+------------------------------------+
                       |+ ${player.name} has chosen to discard the *${CardPosition
              .parse(cardIndex)}* card...
                       |+ ${player
              .hand(cardIndex)} has been added to the DiscardPile.
                       |+------------------------------------+
                       |""".stripMargin)

            GameUtils
              .updatePlayerAndCardDeck(gameState, player, cardIndex)
              .lens(_.discardPile)
              .modify(_.discard(selectedCard))
              .lens(_.hintTokens)
              .modify(_.gain)
          })
    }
  }

}
