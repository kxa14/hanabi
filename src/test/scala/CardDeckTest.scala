import org.scalatest.funsuite.AnyFunSuite

class CardDeckTest extends AnyFunSuite {

  private val cardDeck = new CardDeck

  test("testDeal") {}

  test("testAll") {}

  test("testGenerateDeck") {
    assert(cardDeck.generateDeck.size == 55)
  }

}
