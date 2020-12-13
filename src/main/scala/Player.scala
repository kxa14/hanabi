import CardDeck.Card

// Map(Player, Hand)
// Goal:
case class Player(name: String, hand: Vector[Card]) {}

case class Hand(cards: Vector[Card]) {}
