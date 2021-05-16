package util

import accessories.{Card, CardDeck, Player}
import action.Move
import characteristics.CardPosition
import characteristics.Colours.{Blue, Green, Red, Yellow}
import displayObjects.HintTokens
import org.scalamock.scalatest.MockFactory
import org.scalatest.funspec.AnyFunSpec

final class GameUtilsSpec extends AnyFunSpec with MockFactory {

  describe("askPlayerMove") {
    val stubPlayer = Player(1, "stubPlayer", Vector())
    describe("Given HintTokens's count is not zero") {
      val mockNonZeroHintTokens = HintTokens(1)
      it("should return Hint object when hinting is chosen") {
        assert(
          GameUtils
            .askPlayerMove(stubPlayer, mockNonZeroHintTokens, 'h') == Move.Hint
        )
      }
    }
  }

  describe("updatePlayerHand") {
    val mockPlayer = Player(
      0,
      "mockPlayer",
      Vector(Card(1, Red), Card(2, Yellow), Card(3, Green), Card(4, Blue))
    )
    val stubCard = Card(3, Red)
    val stubCardPosition = CardPosition.Third

    describe("when given a non-empty CardDeck") {
      val stubCardDeck = CardDeck(Vector(stubCard))
      val updatedPlayer =
        GameUtils.updatePlayerHand(mockPlayer, stubCardDeck, stubCardPosition)
      it("should drop the selected card") {
        assert(updatedPlayer.hand.exists(_ != stubCard))
      }
      it("should add a new card to the player's hand") {
        assert(updatedPlayer.hand.last == stubCard)
      }
    }

    describe("when given an empty CardDeck") {
      val mockEmptyCardDeck = CardDeck(Vector[Card]())
      val updatedPlayer =
        GameUtils.updatePlayerHand(
          mockPlayer,
          mockEmptyCardDeck,
          stubCardPosition
        )
      it("should drop the selected card") {
        assert(updatedPlayer.hand.exists(_ != stubCard))
      }
      it(
        "should return the player's hand with size less than one before the call"
      ) {
        assert(updatedPlayer.hand.size == mockPlayer.hand.size - 1)
      }
    }

    describe(
      "Given that a player's hand has more than one card with the same number and colour"
    ) {
      val stubCard = Card(5, Blue)
      val stubCardDeck = CardDeck(Vector(stubCard))
      val cardsWithSameNumAndColour = Card(3, Green)
      val stubCardPosition = CardPosition.Second
      val mockPlayerHand = Player(
        0,
        "mockPlayer",
        Vector(
          Card(1, Red),
          cardsWithSameNumAndColour,
          Card(2, Yellow),
          cardsWithSameNumAndColour
        )
      )
      val updatedPlayer = GameUtils.updatePlayerHand(
        mockPlayerHand,
        stubCardDeck,
        stubCardPosition
      )
      it(
        "should drop the card specified by the Card Position, move the next Card forward and add a new card from CardDeck to the last"
      ) {
        assert(
          updatedPlayer
            .hand(stubCardPosition.toInt) != cardsWithSameNumAndColour
        )
        assert(
          updatedPlayer.hand(stubCardPosition.toInt) == mockPlayerHand
            .hand(stubCardPosition.toInt + 1)
        )
        assert(updatedPlayer.hand.last == stubCard)
      }
    }
  }
}
