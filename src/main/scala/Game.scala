import GameUtils.{askNumberOfPlayers, askPlayerMove, askPlayerNames}
import NumberOfPlayers.{Five, Four, Three, Two, deParse}

class Game {

  val initialSetup: Either[HanabiException, GameState] = {
    askNumberOfPlayers.flatMap(num =>
      askPlayerNames(num).map(names => {
        val numberOfInitialCardsForEachPlayer: Int = num match {
          case Two | Three => 5
          case Four | Five => 4
        }
        val indexes = Vector.range(0, deParse(num) + 1)
        val playersWithEmptyHand: Vector[Player] =
          names.map(Player(_, Vector[Card]()))
        val initialGameState: GameState = {
          indexes.foldRight(GameState(playersWithEmptyHand, CardDeck()))(
            (x, acc) =>
              updateGameState(acc, x, Some(numberOfInitialCardsForEachPlayer))
          )
        }
        initialGameState
      })
    )
  }

  // Player1 will see Player2's hand, decide to hint, put down or trash a card
  // Player1 will key in a char, if invalid, will be asked to key in again -> while/loop
  // If valid, lead to hint(), play(), discard()

  def startPlaying() = {
    initialSetup.flatMap(initialGS => {
      val firstPlayer = initialGS.players.head
      val secondPlayer = initialGS.players(1)
      println(s"""
         |+------------------------------------+
         |+ 
         |+ ${firstPlayer.name}, it's your turn.
         |+ Below are the cards of ${secondPlayer.name}.
         |+ ${secondPlayer.hand}
         |+------------------------------------+
         |""".stripMargin)
      askPlayerMove() match {
        case Left(ex) =>
          println(ex.msg)
          askPlayerMove()
        case Right(move) => move
      }
    })
  }

  def updateGameState(
      gameState: GameState,
      whichPlayer: Int,
      numOfCards: Option[Int]
  ): GameState =
    gameState.copy(
      players = updatePlayerState(
        whichPlayer,
        numOfCards,
        gameState.players,
        gameState.cardDeck
      ),
      cardDeck = updateCardDeckState(gameState.cardDeck, numOfCards)
    )

  private def updatePlayerState(
      whichPlayer: Int,
      numOfCards: Option[Int],
      players: Vector[Player],
      cardDeck: CardDeck
  ): Vector[Player] =
    numOfCards.fold(players)(num =>
      players.updated(whichPlayer, cardDeck.cards.take(num))
    )

  private def updateCardDeckState(
      cardDeck: CardDeck,
      numOfCards: Option[Int]
  ): CardDeck =
    numOfCards.fold(cardDeck)(num =>
      cardDeck.copy(cards = cardDeck.cards.drop(num))
    )

}
