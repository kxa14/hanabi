import accessories.Player
import cats.effect.{ExitCode, IO, IOApp}
import displayObjects.GameState
import hanabi.ConsoleIORequest._
import hanabi.DiscardMoveExecutor.LivePlayersDiscardMoveExecutor
import hanabi.GameStateInitialization.LivePlayersGameStateInitialization
import hanabi.Generator._
import hanabi.HintMoveExecutor.LivePlayersHintMoveExecutor
import hanabi.OnePlayerGameLoop
import hanabi.PlayMoveExecutor.LivePlayersPlayMoveExecutor
import hanabi.Request._

object HanabiProgram extends IOApp {

  private val gameSetup: LivePlayersGameStateInitialization[IO] =
    new LivePlayersGameStateInitialization[IO](
      new RequestNumberOfPlayersImpl[IO](requestNumberOfPlayerProgram),
      new RequestPlayerNamesImpl[IO](requestPlayerNames),
      new LivePlayerGenerator[IO],
      new BasicInitialCardCount[IO],
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
      gameState: IO[GameState],
      players: Vector[Player]
  ): IO[GameState] = {
    def loopAllPlayers(gameState: IO[GameState]): IO[GameState] = {
      players.foldLeft(gameState)((currentGS, currentPlayer) =>
        currentGS.flatMap(onePlayerGameLoop.start(_, currentPlayer))
      )
    }

    gameState.flatMap(gs =>
      if (
        gs.life.count == 0
      ) // TODO: Need to add two more another ending conditions when i) the players complete all five fireworks correctly. ii) a player draws the last card from the draw deck
        IO.println("Game Over! Hope you all had fun :)") *> IO(
          gs
        )
      else repeatCycle(loopAllPlayers(gameState), players)
    )
  }

  private val loop = for {
    initialGS <- gameSetup.generate
    allPlayers <- IO(initialGS.players)
    gs <- allPlayers.foldLeft(IO(initialGS))((currentGS, currentPlayer) =>
      currentGS.flatMap(onePlayerGameLoop.start(_, currentPlayer))
    )
    _ <- repeatCycle(IO(gs), allPlayers)
  } yield ExitCode.Success

  override def run(args: List[String]): IO[ExitCode] = {
    loop
  }
}
