import HanabiException.{InvalidMove, InvalidPlayerNames}
import
import scala.util.{Failure, Success, Try}

object GameUtils {

  def askNumberOfPlayers: Either[HanabiException, NumberOfPlayers] =
    Try(scala.io.StdIn.readInt()) match {
      case Failure(exception) => {
        println(s"Failed. Reason: $exception")
        throw exception
      }
      case Success(value) => NumberOfPlayers.parse(value)
    }

  def askPlayerNames(
      num: NumberOfPlayers
  ): Either[HanabiException, Vector[String]] =
    num match {
      case NumberOfPlayers.Two =>
        askName.flatMap(n1 => askName.map(n2 => Vector(n1, n2)))
      case NumberOfPlayers.Three =>
        askName.flatMap(n1 =>
          askName.flatMap(n2 => askName.map(n3 => Vector(n1, n2, n3)))
        )
      case NumberOfPlayers.Four =>
        askName.flatMap(n1 =>
          askName.flatMap(n2 =>
            askName.flatMap(n3 => askName.map(n4 => Vector(n1, n2, n3, n4)))
          )
        )
      case NumberOfPlayers.Five =>
        askName.flatMap(n1 =>
          askName.flatMap(n2 =>
            askName.flatMap(n3 =>
              askName.flatMap(n4 =>
                askName.map(n5 => Vector(n1, n2, n3, n4, n5))
              )
            )
          )
        )
    }

  def askPlayerMove(): Either[HanabiException, Move] =
    try {
      println(s"""
       |
       | Make a choice: (h)hint, (p)play, (d)discard
       |
       |""".stripMargin)
      Move.parse(scala.io.StdIn.readChar())
    } catch {
      case _: Throwable => Left(InvalidMove())
    }

  private def askName: Either[HanabiException, String] =
    try {
      println(s"What is the name of the player?")
      Right(scala.io.StdIn.readLine())
    } catch {
      case _: Throwable => Left(InvalidPlayerNames())
    }
}
