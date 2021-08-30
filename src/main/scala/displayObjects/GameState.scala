package displayObjects

import accessories.{CardDeck, Player}
import cats.effect.kernel.Sync
import hanabi.OnePlayerGameLoop

final case class GameState(
    players: Vector[Player],
    cardDeck: CardDeck,
    lobby: Lobby,
    discardPile: DiscardPile,
    life: Life,
    hintTokens: HintTokens
) {
  def cycle[F[_]](implicit
      onePlayerGameLoop: OnePlayerGameLoop[F],
      sync: Sync[F]
  ): F[GameState] =
    players.foldLeft(sync.pure(this))((gs, player) =>
      onePlayerGameLoop.start(gs, player)
    )

  def dealCardsToPlayer(playerID: Int, numOfCards: Int): GameState = {
    this.copy(
      players = players.updated(
        playerID, {
          val cards = cardDeck.cards.take(numOfCards)
          players(playerID).copy(hand = cards)
        }
      ),
      cardDeck = cardDeck.copy(cardDeck.cards.drop(numOfCards))
    )
  }

  def display(player: Player): String = {
    val playersToDisplay = players.filterNot(_ == player)
    s"""
       |                            -------------------
       |                            | Life = ${life.count}
       |    --------------          | Hint tokens = ${hintTokens.count}
       |    |   Lobby    |          | Number of CardDeck = ${cardDeck.cards.size}
       |    --------------          -------------------
       |${lobby.showroom.mkString("\n")}
       |
       |    --------------
       |    | Discarded  |
       |    --------------
       |${discardPile.showroom.mkString("\n")}
       |
       |    --------------
       |    |   Players   |
       |    --------------
       |${playersToDisplay.mkString("\n")}
       |
       |--------------------------------------------------------------
       |
       |""".stripMargin
  }
}
