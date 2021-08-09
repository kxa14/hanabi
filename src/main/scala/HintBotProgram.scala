import accessories.Player
import cats.effect.{ExitCode, IO, IOApp}
import displayObjects.{GameState, HintTokens, Life}
import hanabi.ConsoleIORequest._
import hanabi.DiscardMoveExecutor.LivePlayersDiscardMoveExecutor
import hanabi.Generator._
import hanabi.HintMoveExecutor.LivePlayersHintMoveExecutor
import hanabi.PlayMoveExecutor.LivePlayersPlayMoveExecutor
import hanabi.Request._
import hanabi.{GameStateInitialization, OnePlayerGameLoop}
import setup.{NumberOfPlayers, PlayersNames}

object HintBotProgram extends IOApp {
  private val playerNumbers = new NumberOfPlayers[IO] {
    override def run: IO[Int] = IO(2)
  }

  private val playersNames = new PlayersNames[IO] {
    override def run(numOfPlayers: Int): IO[Vector[String]] =
      IO(Vector("Joyful:)", "Brave:)"))
  }

  private val infiniteLifeTokens = new LifeGen[IO] {
    override def generate: IO[Life] = IO(Life(-99))
  }

  private val infiniteHintTokens = new HintTokensGen[IO] {
    override def generate: IO[HintTokens] = IO(HintTokens(-99))
  }

  private val hintBotGameStateInitialization = new GameStateInitialization[IO](
    playerNumbers,
    playersNames,
    new PlayerGenerator[IO],
    new InitialCardCount[IO],
    new BasicCardDeckGen[IO],
    new BasicLobbyGen[IO],
    new BasicDiscardPileGen[IO],
    infiniteLifeTokens,
    infiniteHintTokens
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
    initialGS <- hintBotGameStateInitialization.generate
    allPlayers <- IO(initialGS.players)
    gs <- allPlayers.foldLeft(IO(initialGS))((currentGS, currentPlayer) =>
      currentGS.flatMap(onePlayerGameLoop.start(_, currentPlayer))
    )
    _ <- repeatCycle(IO(gs), allPlayers)
  } yield ExitCode.Success

  override def run(args: List[String]): IO[ExitCode] = loop
}
