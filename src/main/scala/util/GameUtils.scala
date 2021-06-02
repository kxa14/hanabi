package util

import accessories.{CardDeck, Player}
import displayObjects.GameState
import monocle.macros.syntax.lens._

object GameUtils {

  /** For both Play's and Discard's move, each player's hand will need to be updated (dropping and/or adding one card) and
    * CardDeck will need to drop the first card if there is any.
    *
    * @param gameState
    * @param player
    * @param cardPosition
    * @return
    */
  def updatePlayerAndCardDeck(
      gameState: GameState,
      player: Player,
      cardPosition: Int
  ): GameState =
    gameState
      .lens(_.players)
      .modify(
        _.updated(
          player.id,
          GameUtils.updatePlayerHand(player, gameState.cardDeck, cardPosition)
        )
      )
      .lens(_.cardDeck.cards)
      .modify(_.drop(1))

  /** Drops the selected card, then, only if there is still card in the CardDeck, adds one card into the player's hand.
    *
    * @param player
    * @param cardDeck
    * @param cardPosition
    * @return
    */
  def updatePlayerHand(
      player: Player,
      cardDeck: CardDeck,
      cardPosition: Int
  ): Player = {
    val playerWithOneCardLess = player
      .lens(_.hand)
      .modify(
        _.zipWithIndex
          .filterNot(_._2 == cardPosition)
          .map(_._1)
      )
    cardDeck.cards.headOption
      .map(card => playerWithOneCardLess.lens(_.hand).modify(_ :+ card))
      .getOrElse(playerWithOneCardLess)
  }
}
