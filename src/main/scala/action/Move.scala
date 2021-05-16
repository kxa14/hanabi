package action

import accessories.Player
import characteristics.{CardPosition, Clue}
import displayObjects.GameState
import errors.HanabiErrors
import errors.HanabiErrors._
import util.{GameUtils, MultipleCardOptions, SingleCardOptions}

sealed trait Move extends Product with Serializable {
  def execute(gameState: GameState, player: Player): GameState
}

object Move {

  final case object Hint extends Move {
    def execute(gameState: GameState, hintGiver: Player): GameState = {
      val hintReceiver = GameUtils.whichPlayerToHint(gameState, hintGiver)
      val typeOfClue = Clue.hintColourOrNumber
      val clue = typeOfClue.what
      if (typeOfClue.isMoreThanOneCard) {
        val cardPositions =
          GameUtils.whichCards(MultipleCardOptions.parse(gameState.players))
        val afterHintGS = gameState.updateAfterHintMove
        println(s"""|+------------------------------------+
                    |+ ${hintReceiver.name}, ${hintGiver.name} hints that
                    |+ ${cardPositions.mkString(", ")} cards are * $clue *.
                    |
                    |(${afterHintGS.hintTokens.count} hint tokens remaining...)
                    |+------------------------------------+
                    |""".stripMargin)
        afterHintGS
      } else {
        val cardPosition =
          GameUtils.whichCard(SingleCardOptions.parse(gameState.players))
        val afterHintGS = gameState.updateAfterHintMove
        println(s"""|+------------------------------------+
                    |+ ${hintReceiver.name}, ${hintGiver.name} hints that
                    |+ $cardPosition card is * $clue *.
                    |
                    |(${afterHintGS.hintTokens.count} hint tokens remaining...)
                    |+------------------------------------+
                    |""".stripMargin)
        afterHintGS
      }
    }
  }

  case object Play extends Move {
    def execute(
        gs: GameState,
        player: Player
    ): GameState = {
      val cardOptions = SingleCardOptions.parse(gs.players)
      val cardToBePlayed: CardPosition =
        GameUtils.whichCard(cardOptions)
      println(
        s"""|+------------------------------------+
            |+ ${player.name} has chosen to play the *$cardToBePlayed* card...""".stripMargin
      )
      gs.updateAfterPlayMove(player, cardToBePlayed)
    }
  }

  case object Discard extends Move {
    def execute(
        gs: GameState,
        player: Player
    ): GameState = {
      val cardToBeDiscarded: CardPosition =
        GameUtils.whichCard(SingleCardOptions.parse(gs.players))
      println(
        s"""|+------------------------------------+
            |+ ${player.name} has chosen to discard the *$cardToBeDiscarded* card...
            |+ ${player
          .hand(cardToBeDiscarded.toInt)} has been added to the DiscardPile.
            |+------------------------------------+""".stripMargin
      )
      gs.updateAfterDiscardMove(player, cardToBeDiscarded)
    }
  }

  def parse(inputChar: Char): Either[HanabiErrors, Move] = {
    inputChar.toLower match {
      case 'h' => Right(Hint)
      case 'p' => Right(Play)
      case 'd' => Right(Discard)
      case _   => Left(InvalidMove())
    }
  }

  def parseNoHint(inputChar: Char): Either[HanabiErrors, Move] = {
    inputChar.toLower match {
      case 'p' => Right(Play)
      case 'd' => Right(Discard)
      case 'h' => Left(OutOfHintTokens())
      case _   => Left(InvalidMove())
    }
  }
}
