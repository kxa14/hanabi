package characteristics

import cats.data._
import cats.implicits._
import characteristics.CardPosition.CardPositionError.{
  InvalidCardPosition,
  NotAnInteger
}
import errors.HanabiErrors
import errors.HanabiErrors.InvalidCardPositions

sealed trait CardPosition
    extends Ordered[CardPosition]
    with Product
    with Serializable {
  def toInt: Int =
    this match {
      case CardPosition.First  => 0
      case CardPosition.Second => 1
      case CardPosition.Third  => 2
      case CardPosition.Fourth => 3
      case CardPosition.Fifth  => 4
    }

  override def compare(that: CardPosition): Int = this.toInt compare that.toInt
}
object CardPosition {

  case object First extends CardPosition

  case object Second extends CardPosition

  case object Third extends CardPosition

  case object Fourth extends CardPosition

  case object Fifth extends CardPosition

  sealed trait CardPositionError extends HanabiErrors

  object CardPositionError {

    final case class NotAnInteger(v: String) extends CardPositionError {
      def msg: String = s"$v is not an Integer. Expected a number. Try again."
      override def toString: String = s"$msg"
    }

    final case class InvalidCardPosition(num: Int) extends CardPositionError {
      def msg: String =
        s"Error: $num is invalid card number position. Try again."
      override def toString: String = s"$msg"
    }
  }

  def parseFiveCards(
      cardPosition: Int
  ): Either[InvalidCardPosition, CardPosition] =
    cardPosition match {
      case 1 => Right(First)
      case 2 => Right(Second)
      case 3 => Right(Third)
      case 4 => Right(Fourth)
      case 5 => Right(Fifth)
      case _ => Left(InvalidCardPosition(cardPosition))
    }

  def parseFourCards(
      cardPosition: Int
  ): Either[InvalidCardPosition, CardPosition] = {
    cardPosition match {
      case 1 => Right(First)
      case 2 => Right(Second)
      case 3 => Right(Third)
      case 4 => Right(Fourth)
      case _ => Left(InvalidCardPosition(cardPosition))
    }
  }

  def parseMultipleCards(cardNums: String)(
      f: Int => Either[InvalidCardPosition, CardPosition]
  ): Either[HanabiErrors, List[CardPosition]] = {
    val userInput: Either[InvalidCardPositions, NonEmptyList[String]] = cardNums
      .filterNot(_.isWhitespace)
      .split(",")
      .toList
      .toNel
      .toRight(
        HanabiErrors.InvalidCardPositions(
          cardNums,
          "Expected comma separated fields, e.g. 1,2,3"
        )
      )

    val eithers: Either[HanabiErrors, List[CardPosition]] =
      userInput.flatMap(usrInput => {
        val (left, rights) = usrInput
          .map(s => s.toIntOption.toRight(NotAnInteger(s)))
          .map(x => x.flatMap(y => f(y)))
          .toList // TODO: Is there a way to split the Eithers without converting from NonEmptyList to List?
          .partitionMap(identity)
        left.headOption.toLeft(rights.distinct.sorted)
      })
    eithers
  }

}
