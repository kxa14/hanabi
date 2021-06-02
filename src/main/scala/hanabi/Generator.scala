package hanabi

import cats.implicits._
import accessories.{Card, CardDeck, Player}
import cats.Applicative
import cats.effect.kernel.Sync
import displayObjects.{DiscardPile, HintTokens, Life, Lobby}

object Generator {

  /** Tagless Final Algebra: initializes players */
  sealed trait PlayerGenerator[F[_]] {
    def generate(names: Vector[String]): F[Vector[Player]]
  }

  final class LivePlayerGenerator[F[_]: Sync] extends PlayerGenerator[F] {
    override def generate(names: Vector[String]): F[Vector[Player]] = {
      Sync[F].delay(names.zipWithIndex.map {
        case (name, id) => Player(id, name, Vector[Card]())
      })
    }
  }

  sealed trait InitialCardCount[F[_]] {
    def count(numOfPlayers: Int): F[Int]
  }

  final class BasicInitialCardCount[F[_]: Applicative]
      extends InitialCardCount[F] {
    override def count(numOfPlayers: Int): F[Int] =
      if (numOfPlayers <= 3) 5.pure[F] else 4.pure[F]
  }

  sealed trait CardDeckGen[F[_]] {
    def generate: F[CardDeck]
  }
  final class BasicCardDeckGen[F[_]](implicit S: Sync[F])
      extends CardDeckGen[F] {
    override def generate: F[CardDeck] = S.delay(CardDeck())
  }

  sealed trait LobbyGen[F[_]] {
    def generate: F[Lobby]
  }
  final class BasicLobbyGen[F[_]: Sync] extends LobbyGen[F] {
    override def generate: F[Lobby] = Sync[F].delay(Lobby())
  }

  sealed trait DiscardPileGen[F[_]] {
    def generate: F[DiscardPile]
  }
  final class BasicDiscardPileGen[F[_]: Sync] extends DiscardPileGen[F] {
    override def generate: F[DiscardPile] = Sync[F].delay(DiscardPile())
  }

  sealed trait LifeGen[F[_]] {
    def generate: F[Life]
  }
  final class BasicLifeGen[F[_]: Sync] extends LifeGen[F] {

    /**
      * According to rule, the most basic game begins with 3 life tokens.
      * @return
      */
    override def generate: F[Life] = Sync[F].delay(Life(3))
  }

  sealed trait HintTokensGen[F[_]] {
    def generate: F[HintTokens]
  }
  final class BasicHintTokensGen[F[_]: Sync] extends HintTokensGen[F] {

    /**
      * According to rule, the most basic game begins with 8 hint tokens.
      * @return
      */
    override def generate: F[HintTokens] = Sync[F].delay(HintTokens(8))
  }

}
