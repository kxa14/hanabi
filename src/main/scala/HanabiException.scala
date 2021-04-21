trait HanabiException extends Exception with Product with Serializable {
  def msg: String
}

object HanabiException {
  final case class InvalidNumberOfPlayers(
      msg: String =
        s"_ is invalid number of players. \n Minimum = 2 and Maximum = 5"
  ) extends HanabiException

  final case class InvalidPlayerNames(
      msg: String = s"Invalid player name. Please try again."
  ) extends HanabiException

  final case class InvalidMove(
      msg: String = s"""
      | Invalid move. Please try again
      | Make a choice: (h) hint, (p) play, (d) discard
      |
      |""".stripMargin
  ) extends HanabiException

  final case class InvalidHintMove(
      msg: String = "Invalid Hint Move. Try again."
  ) extends HanabiException

  final case class InvalidColour(msg: String = "Invalid Colour. Try again")
      extends HanabiException

//  final case class InvalidCardNumbers(
//      msg: String = "Invalid Card Numbers. Try again"
//  ) extends HanabiException

  final case class InvalidYesNo(
      msg: String = "Invalid yes/no response. Try again"
  ) extends HanabiException

}
