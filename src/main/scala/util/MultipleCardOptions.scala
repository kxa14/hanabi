package util

import accessories.Player
import characteristics.CardPosition
import errors.HanabiErrors

sealed trait MultipleCardOptions {
  def list(
      cardPositionNums: => String
  ): Either[HanabiErrors, List[CardPosition]]
}

object MultipleCardOptions {
  case class LessThanFourPlayers() extends MultipleCardOptions {
    override def list(
        cardPositionNums: => String
    ): Either[HanabiErrors, List[CardPosition]] = {
      val message = s"""
                       | Which cards? For multiple cards, use comma(,), e.g. 2,4,5
                       | (1)First (2)Second (3)Third (4)Fourth (5)Fifth
                       |""".stripMargin
      println(message)
      CardPosition.parseMultipleCards(cardPositionNums)(
        CardPosition.parseFiveCards
      )
    }
  }

  case class MoreThanThreePlayers() extends MultipleCardOptions {
    override def list(
        cardPositionNums: => String
    ): Either[HanabiErrors, List[CardPosition]] = {
      val message = s"""
                       | Which cards? For multiple cards, use comma(,), e.g. 1,2,4
                       | (1)First (2)Second (3)Third (4)Fourth
                       |""".stripMargin
      println(message)
      CardPosition.parseMultipleCards(cardPositionNums)(
        CardPosition.parseFourCards
      )
    }
  }

  def parse(players: Vector[Player]): MultipleCardOptions =
    if (players.size > 3) MoreThanThreePlayers() else LessThanFourPlayers()
}
