package errors

trait HanabiErrors extends Product with Serializable {
  def msg: String
}

object HanabiErrors {
  final case class InvalidNumberOfPlayers(num: Int) extends HanabiErrors {
    override def msg: String =
      s"Error: $num is invalid number of players. Minimum = 2 and Maximum = 5 \n Try again."

    override def toString: String = s"$msg"
  }

  final case class InvalidMove(
      msg: String = s"""
      | Invalid move. Please try again
      | Make a choice: (h) hint, (p) play, (d) discard
      |
      |""".stripMargin
  ) extends HanabiErrors

  final case class OutOfHintTokens(
      msg: String = "Error: Only play or discard is allowed. Please try again."
  ) extends HanabiErrors

  final case class InvalidHintMove(
      msg: String = "Invalid Hint action. Try again."
  ) extends HanabiErrors

  final case class ReadColourHint(
      msg: String = "Invalid Colour. Try again"
  ) extends HanabiErrors

  final case class InvalidYesNo(
      msg: String = "Invalid yes/no response. Try again"
  ) extends HanabiErrors

  final case class ReadCharErrors(
      msg: String = "Input not a Character. Try again"
  ) extends HanabiErrors

  final case class InvalidCardPositions(text: String, msg: String)
      extends HanabiErrors

  final case class ReadIntErrors(
      msg: String = "Input not an Integer. Try again"
  ) extends HanabiErrors

  final case class ReadStringErrors(
      msg: String = "Expected String. Try again"
  ) extends HanabiErrors

  final case class InvalidCardNumber(invalidCardNum: Int) extends HanabiErrors {
    override def msg: String =
      s"Error: $invalidCardNum is invalid card number. \n Expected value is (1 to 5)"

    override def toString: String = s"$msg"
  }

  final case class InvalidPlayerID(invalidId: Int) extends HanabiErrors {
    override def msg: String =
      s"Error: $invalidId is invalid playerID option. \n Try again."

    override def toString: String = s"$msg"
  }
}
