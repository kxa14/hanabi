package hanabi

import accessories.Player
import action.Move
import characteristics.{CardPosition, Clue}
import displayObjects.{GameState, HintTokens}
import setup.{NumberOfPlayers, PlayersNames}

object Request {
  final class RequestNumberOfPlayersImpl[F[_]](nums: => F[Int])
      extends NumberOfPlayers[F] {
    override def run: F[Int] = nums
  }

  final class RequestPlayerNamesImpl[F[_]](names: => Int => F[Vector[String]])
      extends PlayersNames[F] {
    override def run(numOfPlayers: Int): F[Vector[String]] = names(numOfPlayers)
  }

  sealed trait RequestMove[F[_]] {
    def run(player: Player, hintTokens: HintTokens): F[Move]
  }
  final class RequestMoveImpl[F[_]](move: => (Player, HintTokens) => F[Move])
      extends RequestMove[F] {
    override def run(player: Player, hintTokens: HintTokens): F[Move] =
      move(player, hintTokens)
  }

  sealed trait RequestWhoToHint[F[_]] {
    def run(gameState: GameState, hintGiver: Player): F[Player]
  }
  final class RequestWhoToHintImpl[F[_]](
      hintReceiver: => (GameState, Player) => F[Player]
  ) extends RequestWhoToHint[F] {
    override def run(gameState: GameState, hintGiver: Player): F[Player] =
      hintReceiver(gameState, hintGiver)
  }

  sealed trait RequestClue[F[_]] {
    def run: F[Clue]
  }
  final class RequestClueImpl[F[_]](clue: => F[Clue]) extends RequestClue[F] {
    override def run: F[Clue] = clue
  }

  sealed trait RequestColour[F[_]] {
    def run: F[String]
  }
  final class RequestColourImpl[F[_]](colour: => F[String])
      extends RequestColour[F] {
    override def run: F[String] = colour
  }

  sealed trait RequestNumber[F[_]] {
    def run: F[String]
  }
  final class RequestNumberImpl[F[_]](number: => F[String])
      extends RequestNumber[F] {
    override def run: F[String] = number
  }

  sealed trait RequestIsMoreThanOneCard[F[_]] {
    def run(clue: Clue): F[Boolean]
  }
  final class RequestIsMoreThanOneCardImpl[F[_]](yesNo: => Clue => F[Boolean])
      extends RequestIsMoreThanOneCard[F] {
    override def run(clue: Clue): F[Boolean] = yesNo(clue)
  }

  sealed trait RequestSingleCardPosition[F[_]] {
    def run(player: Player): F[Int]
  }
  final class RequestSingleCardPositionImpl[F[_]](
      cardIndex: => Player => F[Int]
  ) extends RequestSingleCardPosition[F] {
    override def run(player: Player): F[Int] = cardIndex(player)
  }

  sealed trait RequestMultipleCardPositions[F[_]] {
    def run(player: Player): F[List[CardPosition]]
  }
  final class RequestMultipleCardPositionsImpl[F[_]](
      cardIndexes: => Player => F[List[CardPosition]]
  ) extends RequestMultipleCardPositions[F] {
    override def run(player: Player): F[List[CardPosition]] =
      cardIndexes(player)
  }
}
