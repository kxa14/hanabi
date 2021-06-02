package hanabi

import cats.implicits._
import cats.Monad
import cats.effect.kernel.Sync

/** Tagless Final algebra */
sealed trait HanabiConsole[F[_], A] { self =>
  def putStrLn(str: String): F[Unit]

  def read: F[A]
}

object HanabiConsole {

  /** Integer Console Interpreter */
  final class StdIntHanabiConsole[F[_]: Sync] extends HanabiConsole[F, Int] {
    override def putStrLn(str: String): F[Unit] = Sync[F].delay(println(str))

    override def read: F[Int] = Sync[F].delay(scala.io.StdIn.readInt)
  }

  /** String Console Interpreter */
  final class StdStringHanabiConsole[F[_]: Sync]
      extends HanabiConsole[F, String] {
    override def putStrLn(str: String): F[Unit] = Sync[F].delay(println(str))

    override def read: F[String] = Sync[F].delay(scala.io.StdIn.readLine)
  }

  /** Char Console Interpreter */
  final class StdCharHanabiConsole[F[_]: Sync] extends HanabiConsole[F, Char] {
    override def putStrLn(str: String): F[Unit] = Sync[F].delay(println(str))

    override def read: F[Char] = Sync[F].delay(scala.io.StdIn.readChar)
  }

  /** Generic Console Ask & Read */
  def request[F[_]: Monad, A](
      question: String
  )(console: HanabiConsole[F, A]): F[A] =
    for {
      _ <- console.putStrLn(question)
      n <- console.read
    } yield n
}
