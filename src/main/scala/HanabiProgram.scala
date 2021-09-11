import cats.effect.{ExitCode, IO, IOApp}
import displayObjects.GameState
import hanabi.ConsoleIORequest._
import hanabi.DiscardMoveExecutor.LivePlayersDiscardMoveExecutor
import hanabi.Generator._
import hanabi.HintMoveExecutor.LivePlayersHintMoveExecutor
import hanabi.PlayMoveExecutor.LivePlayersPlayMoveExecutor
import hanabi.Request._
import hanabi.{GameStateInitialization, OnePlayerGameLoop}

/**
  * Hanabi game for live players without computer bot.
  */
object HanabiProgram extends IOApp {

  private val gameSetup: GameStateInitialization[IO] =
    new GameStateInitialization[IO](
      new RequestNumberOfPlayersImpl[IO](requestNumberOfPlayerProgram),
      new RequestPlayerNamesImpl[IO](requestPlayerNames),
      new PlayerGenerator[IO],
      new InitialCardCount[IO],
      new BasicCardDeckGen[IO],
      new BasicLobbyGen[IO],
      new BasicDiscardPileGen[IO],
      new BasicLifeGen[IO],
      new BasicHintTokensGen[IO]
    )

  private val hintMoveExecutor: LivePlayersHintMoveExecutor[IO] =
    new LivePlayersHintMoveExecutor[IO](
      new RequestWhoToHintImpl[IO](requestWhichPlayerToHint),
      new RequestClueImpl[IO](requestTypeOfClue),
      new RequestColourImpl[IO](requestColourClue),
      new RequestNumberImpl[IO](requestNumberClue),
      new RequestIsMoreThanOneCardImpl[IO](requestIsMoreThanOneCard),
      new RequestSingleCardPositionImpl[IO](requestSingleCardPosition),
      new RequestMultipleCardPositionsImpl[IO](requestMultipleCardPositions)
    )

  private val playMoveExecutor: LivePlayersPlayMoveExecutor[IO] =
    new LivePlayersPlayMoveExecutor[IO](
      new RequestSingleCardPositionImpl[IO](requestSingleCardPosition)
    )
  private val discardMoveExecutor: LivePlayersDiscardMoveExecutor[IO] =
    new LivePlayersDiscardMoveExecutor[IO](
      new RequestSingleCardPositionImpl[IO](requestSingleCardPosition)
    )

  private val onePlayerGameLoop = new OnePlayerGameLoop[IO](
    new RequestMoveImpl[IO](requestMove),
    hintMoveExecutor,
    playMoveExecutor,
    discardMoveExecutor
  )

  private def repeatCycle(
      gameState: IO[GameState]
  )(implicit onePlayerGameLoop: OnePlayerGameLoop[IO]): IO[GameState] = {
    gameState.flatMap(gs =>
      if (gs.life.count == 0)
        IO.println("Game Over! Hope you all had fun :)") *> IO(gs)
      else repeatCycle(gs.cycle)
    )
  }

  override def run(args: List[String]): IO[ExitCode] = {
    repeatCycle(gameSetup.generate)(onePlayerGameLoop) *> IO(ExitCode.Success)
  }
}
