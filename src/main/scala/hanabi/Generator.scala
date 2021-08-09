package hanabi

import cats.implicits._
import accessories.{Card, CardDeck, Player}
import cats.Applicative
import cats.effect.kernel.Sync
import characteristics.Colours
import characteristics.Colours.{Blue, Green, Red, White, Yellow}
import displayObjects.{DiscardPile, HintTokens, Life, Lobby}

object Generator {

  /** Tagless Final Algebra: initializes players */
  final class PlayerGenerator[F[_]: Sync] {
    def generate(names: Vector[String]): F[Vector[Player]] = {
      Sync[F].delay(names.zipWithIndex.map {
        case (name, id) => Player(id, name, Vector[Card]())
      })
    }
  }

  final class InitialCardCount[F[_]: Applicative] {
    def count(numOfPlayers: Int): F[Int] =
      if (numOfPlayers <= 3) 5.pure[F] else 4.pure[F]
  }

  sealed trait CardDeckGen[F[_]] {
    def generate: F[CardDeck]
  }

  /**
    * Generates a card deck with the following criteria:
    * 50 fireworks cards in five colors (red, yellow, white, green, blue)
    * 10 cards per color with the values 1, 1, 1, 2, 2, 3, 3, 4, 4, 5
    *
    * @return
    */
  final class BasicCardDeckGen[F[_]](implicit S: Sync[F])
      extends CardDeckGen[F] {
    override def generate: F[CardDeck] =
      S.delay({
        val deck =
          Vector(1, 1, 1, 2, 2, 3, 3, 4, 4, 5).flatMap(c =>
            Vector(
              Colours.Red,
              Colours.Yellow,
              Colours.White,
              Colours.Green,
              Colours.Blue
            ).map(
              Card(
                c,
                _
              )
            )
          )
        val shuffledDeck = scala.util.Random.shuffle(deck)
        CardDeck(shuffledDeck)
      })
  }

  sealed trait LobbyGen[F[_]] {
    def generate: F[Lobby]
  }
  final class BasicLobbyGen[F[_]: Sync] extends LobbyGen[F] {
    override def generate: F[Lobby] =
      Sync[F].delay(
        Lobby(
          Map(
            Red -> Vector[Int](),
            Yellow -> Vector[Int](),
            Green -> Vector[Int](),
            Blue -> Vector[Int](),
            White -> Vector[Int]()
          )
        )
      )
  }

  sealed trait DiscardPileGen[F[_]] {
    def generate: F[DiscardPile]
  }
  final class BasicDiscardPileGen[F[_]: Sync] extends DiscardPileGen[F] {
    override def generate: F[DiscardPile] =
      Sync[F].delay(
        DiscardPile(
          Map(
            Red -> Vector[Int](),
            Yellow -> Vector[Int](),
            Green -> Vector[Int](),
            Blue -> Vector[Int](),
            White -> Vector[Int]()
          )
        )
      )
  }

  trait LifeGen[F[_]] {
    def generate: F[Life]
  }

  final class BasicLifeGen[F[_]: Sync] extends LifeGen[F] {

    /**
      * According to rule, the most basic game begins with 3 life tokens.
      * @return
      */
    override def generate: F[Life] = Sync[F].delay(Life(3))
  }

  trait HintTokensGen[F[_]] {
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
