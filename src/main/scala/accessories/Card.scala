package accessories

import characteristics.Colours
import errors.HanabiErrors.InvalidCardNumber

final case class Card(num: Int, colour: Colours)

object Card {
  def parse(num: Int): Either[InvalidCardNumber, String] =
    Either.cond(num >= 1 || num <= 5, num.toString, InvalidCardNumber(num))
}
