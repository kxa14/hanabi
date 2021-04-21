import CardDeck.generateDeck
import NumberOfPlayers.{Five, Four, Three, Two, deParse}
// 55 cards => a deck for each game, shuffle randomly
// 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5

final case class CardDeck(cards: Vector[Card]) {

  def deal(numOfCards: Int): Vector[Card] = this.cards.take(numOfCards)

}

object CardDeck {

  def apply(): CardDeck = generateDeck

  private def generateDeck: CardDeck = {
    val deck =
      Vector(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5).flatMap(c =>
        Vector(
          Colours.Red,
          Colours.Yellow,
          Colours.White,
          Colours.Green,
          Colours.Blue
        ).map(
          Card(
            c,
            _
          )
        )
      )
    val shuffledDeck = scala.util.Random.shuffle(deck)
    new CardDeck(shuffledDeck)
  }
}
