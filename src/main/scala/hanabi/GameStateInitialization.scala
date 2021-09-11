package hanabi

import cats.Monad
import cats.effect.kernel.Sync
import cats.implicits._
import displayObjects.GameState
import hanabi.Generator._
import setup.{NumberOfPlayers, PlayersNames}

/** Kick starts the game by initializing the game state.
  * Makes it concrete because the steps of initializing the game are the same
  * regardless the player is real human being or a computer bot.
  * Just needs to plug in a different implementations of getting player headcount and names.
  *
  * @param numberOfPlayers
  * @param playerNames
  * @param playerGenerator
  * @param initialCardCount
  * @param cardDeckGen
  * @param lobbyGen
  * @param discardPileGen
  * @param lifeGen
  * @param hintTokensGen
  * @param monad$F$0
  * @param sync$F$1
  * @tparam F
  */
final class GameStateInitialization[F[_]: Monad: Sync](
    numberOfPlayers: NumberOfPlayers[F],
    playerNames: PlayersNames[F],
    playerGenerator: PlayerGenerator[F],
    initialCardCount: InitialCardCount[F],
    cardDeckGen: CardDeckGen[F],
    lobbyGen: LobbyGen[F],
    discardPileGen: DiscardPileGen[F],
    lifeGen: LifeGen[F],
    hintTokensGen: HintTokensGen[F]
) {
  def generate: F[GameState] =
    for {
      numOfPlayers <- numberOfPlayers.run
      names <- playerNames.run(numOfPlayers)
      players <- playerGenerator.generate(names)
      _ <- Sync[F].delay(println("Initializing Hanabi game state...\n"))
      cardNum <- initialCardCount.count(numOfPlayers)
      cardDeck <- cardDeckGen.generate
      lobby <- lobbyGen.generate
      discardPile <- discardPileGen.generate
      life <- lifeGen.generate
      hintTokens <- hintTokensGen.generate
      initialGS <- Sync[F].delay(
        GameState(players, cardDeck, lobby, discardPile, life, hintTokens)
      )
      _ <- Sync[F].delay(println("Dealing cards to each player...\n")).void
      secondGS <- Sync[F].delay(
        players
          .map(_.id)
          .foldRight(initialGS)((id, gs) => gs.dealCardsToPlayer(id, cardNum))
      )
      _ <-
        Sync[F]
          .delay(println("Hanabi game setup completed...\nLet's play! "))
          .void
    } yield secondGS
}
