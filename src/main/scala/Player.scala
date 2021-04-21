import scala.collection.View.Empty

// Map(Player, Hand)
// Goal:
case class Player(name: String, hand: Vector[Card]) {

  def hint(): Hand = ???
  def play(): Hand = ???
  def discard(): Hand = ???
}

case class Hand(cards: Vector[Card]) {}
