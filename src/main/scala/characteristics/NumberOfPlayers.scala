package characteristics

import errors.HanabiErrors

sealed trait NumberOfPlayers extends Product with Serializable {
  def toInt: Int =
    this match {
//      case NumberOfPlayers.One   => 1
      case NumberOfPlayers.Two   => 2
      case NumberOfPlayers.Three => 3
      case NumberOfPlayers.Four  => 4
      case NumberOfPlayers.Five  => 5
    }
}

object NumberOfPlayers {
//  case object One extends characteristics.NumberOfPlayers
  case object Two extends NumberOfPlayers
  case object Three extends NumberOfPlayers
  case object Four extends NumberOfPlayers
  case object Five extends NumberOfPlayers

  def parse(num: Int): Either[HanabiErrors, NumberOfPlayers] =
    num match {
      //    case 1 => Right(One)
      case 2 => Right(Two)
      case 3 => Right(Three)
      case 4 => Right(Four)
      case 5 => Right(Five)
      case _ => Left(HanabiErrors.InvalidNumberOfPlayers(num))
    }

  def deParse(num: NumberOfPlayers): Int =
    num match {
//    case NumberOfPlayers.One => 1
      case NumberOfPlayers.Two   => 2
      case NumberOfPlayers.Three => 3
      case NumberOfPlayers.Four  => 4
      case NumberOfPlayers.Five  => 5
    }
}
