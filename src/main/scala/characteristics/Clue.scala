package characteristics

import accessories.Card
import errors.HanabiErrors
import errors.HanabiErrors.{InvalidHintMove, ReadCharErrors, ReadIntErrors}
import util.GameUtils

sealed trait Clue {
  def typeOfClue: String
  def what: String
  def isMoreThanOneCard: Boolean =
    try {
      println(s"""
                 | Are there more than one card with the same $typeOfClue?
                 | (y)Yes (n)No
                 |""".stripMargin)
      GameUtils.parseYesNo(scala.io.StdIn.readChar().toLower) match {
        case Left(err) =>
          println(err)
          isMoreThanOneCard
        case Right(yesNo) => yesNo
      }
    } catch {
      case _: Throwable =>
        println(ReadCharErrors())
        isMoreThanOneCard
    }
}

object Clue {
  final case class Colour(typeOfClue: String = "colour") extends Clue {
    def what: String =
      try {
        println(s"""
             | What colour?
             | (r)Red (y)Yellow (w)White (g)Green (b)Blue
             |""".stripMargin)
        Colours.parse(scala.io.StdIn.readChar().toLower) match {
          case Left(err) =>
            println(err)
            what
          case Right(oneColour) => oneColour.value
        }
      } catch {
        case _: Throwable =>
          println(ReadCharErrors())
          what
      }
  }

  final case class Number(typeOfClue: String = "number") extends Clue {
    def what: String =
      try {
        println(s"""
                   | What number?
                   | (1) (2) (3) (4) (5)
                   |""".stripMargin)
        Card.parse(scala.io.StdIn.readInt()) match {
          case Left(err) =>
            println(err)
            what
          case Right(numClue) => numClue
        }
      } catch {
        case _: Throwable =>
          println(ReadIntErrors())
          what
      }
  }

  def hintColourOrNumber: Clue =
    try {
      println(s"To hint (c)Colour or (n)Number?")
      Clue.parse(scala.io.StdIn.readChar().toLower) match {
        case Left(err) =>
          println(s"$err")
          hintColourOrNumber
        case Right(hint) => hint
      }
    } catch {
      case _: Throwable =>
        println(ReadCharErrors())
        hintColourOrNumber
    }

  private def parse(inputChar: Char): Either[HanabiErrors, Clue] =
    inputChar match {
      case 'c' => Right(Clue.Colour())
      case 'n' => Right(Clue.Number())
      case _   => Left(InvalidHintMove())
    }

}
