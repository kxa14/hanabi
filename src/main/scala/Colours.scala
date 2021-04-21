import HanabiException.InvalidColour

sealed trait Colours extends Product with Serializable {
  def print: String
}

object Colours {
  case object Red extends Colours {
    override def print: String = s""
  }
  case object Yellow extends Colours
  case object White extends Colours
  case object Green extends Colours
  case object Blue extends Colours

  def parse(char: Char): Either[HanabiException, Colours] = {
    char match {
      case 'r' => Right(Red)
      case 'y' => Right(Yellow)
      case 'w' => Right(White)
      case 'g' => Right(Green)
      case 'b' => Right(Blue)
      case _   => Left(InvalidColour())
    }
  }
}
