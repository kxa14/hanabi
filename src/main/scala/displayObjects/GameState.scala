package displayObjects

import accessories.{Card, CardDeck, Player}
import characteristics.NumberOfPlayers.{Five, Four, Three, Two}
import characteristics.{CardPosition, NumberOfPlayers}
import util.GameUtils
import util.GameUtils.{askNumberOfPlayers, askPlayerMove, askPlayerNames}
import monocle.macros.syntax.lens._

final case class GameState(
    players: Vector[Player],
    cardDeck: CardDeck,
    lobby: Lobby,
    discardPile: DiscardPile,
    life: Life,
    hintTokens: HintTokens
) {

  def cycle: GameState =
    players
      .map(_.id)
      .foldLeft(this)(op =
        (gs, playerId) =>
          if (gs.life.count == 0) {
            gs
          } else {
            println(gs.show(gs.players(playerId)))
            askPlayerMove(
              gs.players(playerId),
              hintTokens,
              scala.io.StdIn.readChar()
            ).execute(gs, gs.players(playerId))
          }
      )

  def show(currentPlayer: Player): String = {
    val playersToDisplay = players.filterNot(_ == currentPlayer)
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

  /** Reduces hint token by one.
    *
    * @return
    */
  def updateAfterHintMove: GameState = this.copy(hintTokens = hintTokens.lose)

  /** If the card is played correctly, adds the card to the Lobby, then
    * drops the card from the player's hand and adds a new card to it from the accessories.CardDeck.
    * Else, adds the card to the DiscardPile and reduces Life's count by one.
    *
    * @param player
    * @param cardPosition
    * @return
    */
  def updateAfterPlayMove(
      player: Player,
      cardPosition: CardPosition
  ): GameState = {
    val selectedCard: Card = player.hand(cardPosition.toInt)
    val updatedPlayerAndCardDeck: GameState =
      GameUtils.updatePlayerAndCardDeck(this, player, cardPosition)

    if (updatedPlayerAndCardDeck.lobby.isCorrectCard(selectedCard)) {
      updatedPlayerAndCardDeck.lens(_.lobby).modify(_.add(selectedCard))
    } else {
      updatedPlayerAndCardDeck
        .lens(_.discardPile)
        .modify(_.discard(selectedCard))
        .lens(_.life.count)
        .modify(_ - 1)
    }
  }

  /** Adds the selected card to the DiscardPile, drops the card from the player's hand and then adds a new card to it from the accessories.CardDeck.
    * Finally, increases the hint token by one.
    *
    * @param player
    * @param cardPosition
    * @return
    */
  def updateAfterDiscardMove(
      player: Player,
      cardPosition: CardPosition
  ): GameState = {
    val selectedCard: Card = player.hand(cardPosition.toInt)
    GameUtils
      .updatePlayerAndCardDeck(this, player, cardPosition)
      .lens(_.discardPile)
      .modify(_.discard(selectedCard))
      .lens(_.hintTokens)
      .modify(_.gain)
  }

  private def dealCardsToPlayer(playerID: Int, numOfCards: Int): GameState = {
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
}

object GameState {

  def apply(): GameState = {
    println("Setting up Hanabi game...")
    val numOfPlayers: NumberOfPlayers = askNumberOfPlayers
    val playerNames: Vector[String] = askPlayerNames(numOfPlayers)
    val numberOfInitialCardsForEachPlayer: Int = numOfPlayers match {
      case Two | Three => 5
      case Four | Five => 4
    }
    val playersWithEmptyHand: Vector[Player] =
      playerNames.zipWithIndex.map(p => Player(p._2, p._1, Vector[Card]()))
    val playerIDs: Vector[Int] = playersWithEmptyHand.map(_.id)
    println(s"""
         |
         |Initializing Hanabi game state...""".stripMargin)
    val initialGS: GameState = GameState.initialise(playersWithEmptyHand)
    println("Hanabi game setup completed...")
    println("Dealing cards to each player...")
    playerIDs.foldRight(initialGS)((x, gs) =>
      gs.dealCardsToPlayer(x, numberOfInitialCardsForEachPlayer)
    )
  }

  private def initialise(players: Vector[Player]): GameState =
    GameState(
      players,
      CardDeck(),
      Lobby(),
      DiscardPile(),
      Life(),
      HintTokens()
    )
}
