package accessories

import characteristics.Colours

final case class CardDeck(cards: Vector[Card])

object CardDeck {

  def apply(): CardDeck = generateDeck

  /**
    * Generates a card deck with the following criteria:
    * 50 fireworks cards in five colors (red, yellow, white, green, blue)
    * 10 cards per color with the values 1, 1, 1, 2, 2, 3, 3, 4, 4, 5
    *
    * @return
    */
  private def generateDeck: CardDeck = {
    val deck =
      Vector(1, 1, 1, 2, 2, 3, 3, 4, 4, 5).flatMap(c =>
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
    CardDeck(shuffledDeck)
  }
}
