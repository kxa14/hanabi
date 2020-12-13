// 55 cards => a deck for each game, shuffle randomly
// 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5

private final class CardDeck {

  import CardDeck.Card

  def all(): Vector[Card] = ???

  // deal 5 cards to each player at the start of the game
  // reduce mainDeck by 5* no. of player
  def deal: Vector[Card] = ???

  def generateDeck: Vector[Card] = {
    val deck = Vector(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5).flatMap(c =>
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
    scala.util.Random.shuffle(deck)
  }

}

object CardDeck {

  final case class Card(num: Int, colour: Colours)

}
