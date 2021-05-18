package util

import accessories.Player
import characteristics.CardPosition
import characteristics.CardPosition.CardPositionError.InvalidCardPosition

sealed trait SingleCardOptions {
  def list(cardPositionNum: => Int): Either[InvalidCardPosition, CardPosition]
}

object SingleCardOptions {
  case class LessThanFourPlayers() extends SingleCardOptions {
    override def list(
        cardPositionNum: => Int
    ): Either[InvalidCardPosition, CardPosition] = {
      val message = s"""
                       | Which card?
                       | (1)First (2)Second (3)Third (4)Fourth (5)Fifth
                       |""".stripMargin
      println(message)
      CardPosition.parseFiveCards(cardPositionNum)
    }
  }

  case class MoreThanThreePlayers() extends SingleCardOptions {
    override def list(
        cardPositionNum: => Int
    ): Either[InvalidCardPosition, CardPosition] = {
      val message = s"""
                       | Which card?
                       | (1)First (2)Second (3)Third (4)Fourth
                       |""".stripMargin
      println(message)
      CardPosition.parseFourCards(cardPositionNum)
    }
  }

  def parse(players: Vector[Player]): SingleCardOptions =
    if (players.size > 3) MoreThanThreePlayers() else LessThanFourPlayers()
}
