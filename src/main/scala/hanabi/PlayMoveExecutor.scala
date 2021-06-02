package hanabi

import accessories.{Card, Player}
import cats.Applicative
import cats.implicits._
import characteristics.CardPosition
import displayObjects._
import hanabi.Request.RequestSingleCardPosition
import monocle.macros.syntax.lens._
import util.GameUtils

sealed trait PlayMoveExecutor[F[_]] {
  def execute(
      gameState: GameState,
      player: Player
  ): F[GameState]
}

object PlayMoveExecutor {

  final class LivePlayersPlayMoveExecutor[F[_]: Applicative](
      requestSingleCardPosition: RequestSingleCardPosition[F]
  ) extends PlayMoveExecutor[F] {
    override def execute(
        gameState: GameState,
        player: Player
    ): F[GameState] = {
      if (player.hand.isEmpty) gameState.pure[F]
      else {
        requestSingleCardPosition
          .run(player)
          .map(cardIndex => {
            val selectedCard: Card = player.hand(cardIndex)
            println(s"""
                 |+------------------------------------+
                 |+ ${player.name} has chosen to play the *${CardPosition
              .parse(cardIndex)}* card...
                 |""".stripMargin)
            val gsAfterUpdatingPlayerAndCardDeck =
              GameUtils.updatePlayerAndCardDeck(gameState, player, cardIndex)
            if (
              gsAfterUpdatingPlayerAndCardDeck.lobby.isCorrectCard(selectedCard)
            )
              gsAfterUpdatingPlayerAndCardDeck
                .lens(_.lobby)
                .modify(_.add(selectedCard))
            else
              gsAfterUpdatingPlayerAndCardDeck
                .lens(_.discardPile)
                .modify(_.discard(selectedCard))
                .lens(_.life.count)
                .modify(_ - 1)
          })
      }
    }
  }
}
