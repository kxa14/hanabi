import CardNumber.CardNumberError.{
  InvalidCardNumber,
  NotAnInteger,
  TooFewCardNumbers,
  TooManyCardNumbers
}

sealed trait CardNumber extends Product with Serializable

object CardNumber {
  case object First extends CardNumber
  case object Second extends CardNumber
  case object Third extends CardNumber
  case object Fourth extends CardNumber
  case object Fifth extends CardNumber

  sealed trait CardNumberError extends HanabiException
//  sealed trait CardNumberError

  object CardNumberError {
    final case class TooManyCardNumbers(private val size: Int)
        extends CardNumberError {
      def msg: String =
        s"You input $size number of cards. Too many, minimum = 1, maximum = 5"
    }
    final case object TooFewCardNumbers extends CardNumberError {
      def msg: String =
        s"Zero number of cards is not allowed. Too few, minimum = 1, maximum = 5"
    }

    final case class NotAnInteger(v: Vector[String]) extends CardNumberError {
      def msg: String = s"$v contains non-Integer values."
    }

    final case class InvalidCardNumbers(private val cardNums: Vector[String])
        extends CardNumberError {
      def msg: String = s"$cardNums are invalid card numbers. Try again"
    }

//    final case class InvalidCardNumber(private val cardNum: Int)
    final case object InvalidCardNumber extends CardNumberError {
      def msg: String = s" is invalid card number. Try again"
//      def msg: String = s"$cardNum is invalid card number. Try again"
    }

    final case class ReadCardNumberError(ex: Throwable)
        extends CardNumberError {
      override def msg: String = s"$ex"
    }
  }

  def parse(cardNum: Int): Either[CardNumberError, CardNumber] = {
    cardNum match {
      case 1 => Right(First)
      case 2 => Right(Second)
      case 3 => Right(Third)
      case 4 => Right(Fourth)
      case 5 => Right(Fifth)
      case _ => Left(InvalidCardNumber)
    }
  }

  def parse(
      cardNums: String
  ): Either[CardNumberError, Vector[CardNumber]] = {
    val userInput = cardNums.filterNot(_.isWhitespace).split(",").toVector
    if (userInput.size > 5) Left(TooManyCardNumbers(userInput.size))
    else if (userInput.size <= 0) Left(TooFewCardNumbers)
    else {
      val strToInts = userInput.map(s => (s.toIntOption, s)).toMap
      val optionalInvalidCardNums =
        strToInts.filter(_._1.isEmpty).values.toVector
      if (optionalInvalidCardNums.nonEmpty)
        Left(NotAnInteger(optionalInvalidCardNums))
      else {
        val eithers =
          strToInts.map(num => parse(num._1.get)).toVector
        val (lefts, right) = {
          eithers.partitionMap(identity)
        }
        lefts.headOption.toLeft(right)
      }
    }
  }
}
