package hanabi

import accessories.Player
import action.Move
import cats.effect.IO
import cats.implicits._
import characteristics.{CardPosition, Clue}
import characteristics.Clue.{Colour, Number}
import displayObjects.{GameState, HintTokens}
import retry.{
  RetryDetails,
  RetryPolicies,
  RetryPolicy,
  retryingOnAllErrors,
  retryingOnFailuresAndAllErrors
}
import HanabiConsole.{
  StdCharHanabiConsole,
  StdIntHanabiConsole,
  StdStringHanabiConsole,
  request
}

import scala.concurrent.duration.DurationInt

object ConsoleIORequest {

  /** With cats-retry */
  private val retryPolicy: RetryPolicy[IO] =
    RetryPolicies.constantDelay[IO](10.milliseconds)
  private def errorHandler(err: Throwable, details: RetryDetails): IO[Unit] =
    IO.println(s"$err. Try again.")

  /** Parse NumberOfPlayer & Keeping trying until receiving valid number */
  def requestNumberOfPlayerProgram: IO[Int] =
    retryingOnFailuresAndAllErrors(
      retryPolicy,
      (x: Int) => IO.pure(x >= 2 && x <= 5),
      (invalidInt: Int, details: RetryDetails) =>
        IO.println(
          s"$invalidInt is invalid number of players. Minimum = 2 and Maximum = 5."
        ),
      errorHandler
    )(request("How many number of players?")(new StdIntHanabiConsole[IO]))

  /** Request name of players */
  def requestPlayerNames(numOfPlayers: Int): IO[Vector[String]] = {
    def requestName: IO[String] = {
      retryingOnAllErrors(
        retryPolicy,
        errorHandler
      )(
        request("What is the name of the player?")(
          new StdStringHanabiConsole[IO]
        )
      )
    }
    Vector.fill(numOfPlayers)(requestName).sequence
  }

  /** Request move from a player */
  def requestMove(player: Player, hintTokens: HintTokens): IO[Move] = {
    val (noHintMsg, noHintFailureHandler) = (
      s"Make a choice: (p) play, (d) discard",
      (x: Char) => IO.pure(x.toLower == 'p' | x.toLower == 'd')
    )
    val (hintMsg, hintFailureHandler) = (
      s"Make a choice: (h)hint, (p) play, (d) discard",
      (x: Char) =>
        IO.pure(x.toLower == 'h' | x.toLower == 'p' | x.toLower == 'd')
    )

    def charMove(msg: String, f: Char => IO[Boolean]): IO[Move] = {
      retryingOnFailuresAndAllErrors(
        retryPolicy,
        f,
        (invalidMoveChar: Char, details: RetryDetails) =>
          IO.println(
            s"""
               | $invalidMoveChar is invalid Move input. Please try again.
               | $msg
               |
               |""".stripMargin
          ),
        errorHandler
      )(request(s"${player.name}, $msg \n")(new StdCharHanabiConsole[IO]))
    }.map(Move.parse)

    if (hintTokens.count == 0) charMove(noHintMsg, noHintFailureHandler)
    else charMove(hintMsg, hintFailureHandler)
  }

  def requestColourClue: IO[String] = {
    retryingOnFailuresAndAllErrors(
      retryPolicy,
      (x: Char) =>
        IO.pure(
          x.toLower == 'r' || x.toLower == 'y' || x.toLower == 'w' || x.toLower == 'g' || x.toLower == 'b'
        ),
      (invalidChar: Char, details: RetryDetails) =>
        IO.println(
          s"$invalidChar is invalid Colour input. Please try again. \n"
        ),
      errorHandler
    )(request(s"""
         | What colour?
         |  (r)Red (y)Yellow (w)White (g)Green (b)Blue
         |""".stripMargin)(new StdCharHanabiConsole[IO]))
  }.map(Colour.parse)

  def requestNumberClue: IO[String] = {
    retryingOnFailuresAndAllErrors(
      retryPolicy,
      (x: Int) =>
        IO.pure(
          x >= 1 || x <= 5
        ),
      (invalidInt: Int, details: RetryDetails) =>
        IO.println(
          s"$invalidInt is invalid Number input. Please try again. \n"
        ),
      errorHandler
    )(request(s"""
                 | What number?
                 |  (1) (2) (3) (4) (5)
                 |""".stripMargin)(new StdIntHanabiConsole[IO]))
  }.map(Number.parse)

  def requestSingleCardPosition(player: Player): IO[Int] = {
    val availableCardsCount = player.hand.size
    val cardMsg = Map(
      1 -> "(1)First",
      2 -> "(1)First (2)Second",
      3 -> "(1)First (2)Second (3)Third",
      4 -> "(1)First (2)Second (3)Third (4)Fourth",
      5 -> "(1)First (2)Second (3)Third (4)Fourth (5)Fifth"
    )

    retryingOnFailuresAndAllErrors(
      retryPolicy,
      (x: Int) => IO(x > 0 && x <= availableCardsCount),
      (invalidInt: Int, details: RetryDetails) =>
        IO.println(
          s"$invalidInt is invalid card position. Try again."
        ),
      errorHandler
    )(
      request(s"Which card? \n ${cardMsg(availableCardsCount)} ")(
        new StdIntHanabiConsole[IO]
      )
    )
  }.map(_ - 1)

  def requestMultipleCardPositions(
      hintReceiver: Player
  ): IO[List[CardPosition]] = {
    val availableCardsCount = hintReceiver.hand.size
    val cardMsg = Map(
      1 -> "(1)First",
      2 -> "(1)First (2)Second",
      3 -> "(1)First (2)Second (3)Third",
      4 -> "(1)First (2)Second (3)Third (4)Fourth",
      5 -> "(1)First (2)Second (3)Third (4)Fourth (5)Fifth"
    )

    def isCardPositionValid(num: Int): Boolean =
      num > 0 && num <= availableCardsCount

    def wasSuccessful(cardNums: String): IO[Boolean] =
      IO {
        cardNums
          .filterNot(_.isWhitespace)
          .split(",")
          .toList
          .toNel
          .map(numsString =>
            numsString
              .map(numString => numString.toIntOption)
              .map(i => i.exists(isCardPositionValid))
          )
          .exists(x => if (x.exists(_ == false)) false else true)
      }

    retryingOnFailuresAndAllErrors(
      retryPolicy,
      wasSuccessful,
      (invalidCardNumString: String, details: RetryDetails) =>
        IO.println(s"$invalidCardNumString is invalid. Try again."),
      errorHandler
    )(
      request(
        s"Which cards? For multiple cards, use comma(,), e.g. 1,2,4 \n ${cardMsg(availableCardsCount)} "
      )(
        new StdStringHanabiConsole[IO]
      )
    ).map(
      _.filterNot(_.isWhitespace)
        .split(",")
        .toList
        .flatMap(_.map(_.toInt).distinct.sorted)
    ) // can use .toInt directly because check has already been done earlier in wasSuccessful(cardNums: String) method.
  }.map(_.map(_ - 1))
    .map(_.map(CardPosition.parse)) // TODO: Bug, investigating...

  def requestWhichPlayerToHint(
      gameState: GameState,
      hintGiver: Player
  ): IO[Player] = {
    val otherPlayers = gameState.players.filterNot(_ == hintGiver)
    val playerOptionsMsg: String =
      otherPlayers.map(p => s"${p.id})${p.name} ").foldLeft("")(_ + _)

    retryingOnFailuresAndAllErrors(
      retryPolicy,
      (playerIdInput: Int) =>
        IO.pure(otherPlayers.map(_.id).contains(playerIdInput)),
      (invalidPlayerIdInput: Int, details: RetryDetails) =>
        IO.println(
          s"Error: $invalidPlayerIdInput is an invalid playerID option. \n Try again."
        ),
      errorHandler
    )(
      request(
        s"${hintGiver.name}, who do you want to hint? \n $playerOptionsMsg "
      )(new StdIntHanabiConsole[IO])
    ).map(validID => otherPlayers.find(_.id == validID).get)
  }

  def requestTypeOfClue: IO[Clue] =
    retryingOnFailuresAndAllErrors(
      retryPolicy,
      (clueInput: Char) =>
        IO.pure(clueInput.toLower == 'c' || clueInput.toLower == 'n'),
      (invalidClueChar: Char, details: RetryDetails) =>
        IO.println(s"$invalidClueChar is invalid hint action. Try again."),
      errorHandler
    )(request("To hint (c)Colour or (n)Number?")(new StdCharHanabiConsole[IO]))
      .map(_.toLower match {
        case 'c' => Colour()
        case 'n' => Number()
      })

  def requestIsMoreThanOneCard(clue: Clue): IO[Boolean] =
    retryingOnFailuresAndAllErrors(
      retryPolicy,
      (x: Char) => IO.pure(x.toLower == 'y' || x.toLower == 'n'),
      (invalidChar: Char, details: RetryDetails) =>
        IO.println(s"$invalidChar is invalid yes/no response. Try again"),
      errorHandler
    )(
      request(
        s"Are there more than one card with the same ${clue.name}? \n (y)Yes (n)No \n"
      )(new StdCharHanabiConsole[IO])
    ).map(_.toLower match {
      case 'y' => true
      case 'n' => false
    })

}
