package hanabi

import accessories.Player
import cats.Monad
import cats.effect.kernel.Sync
import cats.implicits._
import characteristics.{CardPosition, Clue}
import displayObjects.GameState
import hanabi.Request._
import monocle.macros.syntax.lens._

sealed trait HintMoveExecutor[F[_]] {
  def execute(
      gameState: GameState,
      player: Player
  ): F[GameState]
}

object HintMoveExecutor {
  final class LivePlayersHintMoveExecutor[F[_]: Monad: Sync](
      requestWhoToHint: RequestWhoToHint[F],
      requestClue: RequestClue[F],
      requestColour: RequestColour[F],
      requestNumber: RequestNumber[F],
      requestIsMoreThanOneCard: RequestIsMoreThanOneCard[F],
      requestSingleCardPosition: RequestSingleCardPosition[F],
      requestMultipleCardPositions: RequestMultipleCardPositions[F]
  ) extends HintMoveExecutor[F] {
    def execute(gameState: GameState, player: Player): F[GameState] =
      for {
        hintReceiver <- requestWhoToHint.run(gameState, player)
        clue <- requestClue.run
        guess <- parseClue(clue)
        isMoreThanOneCard <- requestIsMoreThanOneCard.run(clue)
        gs <-
          if (isMoreThanOneCard) for {
            cardIndexes <- requestMultipleCardPositions.run(player)
//            cardPositions <- Sync[F].delay(
//              cardIndexes.map(x => CardPosition.parse(x))
//            ) // TODO: Bug, WIP: not sure why...
            afterHintGS <-
              Sync[F].delay(gameState.lens(_.hintTokens).modify(_.lose))
            _ <- Sync[F].delay {
              println(
                msg(
                  hintReceiver,
                  player,
                  afterHintGS,
                  multipleCardPositionMsg(cardIndexes, guess)
                )
              )
            }.void
          } yield afterHintGS
          else {
            for {
              cardIndex <- requestSingleCardPosition.run(player)
              cardPosition <- Sync[F].delay(CardPosition.parse(cardIndex))
              afterHintGS <-
                Sync[F].delay(gameState.lens(_.hintTokens).modify(_.lose))
              _ <-
                Sync[F]
                  .delay(
                    println(
                      msg(
                        hintReceiver,
                        player,
                        afterHintGS,
                        singleCardPositionMsg(cardPosition, guess)
                      )
                    )
                  )
                  .void
            } yield afterHintGS
          }
      } yield gs

    def parseClue(clue: Clue): F[String] =
      clue match {
        case Clue.Colour(_) => requestColour.run
        case Clue.Number(_) => requestNumber.run
      }

    def multipleCardPositionMsg(
        cardPositions: List[CardPosition],
        guess: String
    ): String =
      s"${cardPositions
        .mkString(", ")} cards are * $guess *."
    def singleCardPositionMsg(
        cardPosition: CardPosition,
        guess: String
    ): String =
      s"$cardPosition card is * $guess *."

    def msg(
        hintReceiver: Player,
        hintGiver: Player,
        afterHintGS: GameState,
        cardPositionMsg: String
    ): String =
      s"""
         |+------------------------------------+
         |+ ${hintReceiver.name}, ${hintGiver.name} hints that
         |+ $cardPositionMsg
         |+
         |+ (${afterHintGS.hintTokens.count} hint tokens remaining...)
         |+------------------------------------+
         |""".stripMargin
  }

}
