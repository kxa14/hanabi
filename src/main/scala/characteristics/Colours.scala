package characteristics
import errors.HanabiErrors
import errors.HanabiErrors.ReadColourHint

sealed trait Colours extends Product with Serializable {
  def value: String
}

object Colours {
  case object Red extends Colours {
    override def value: String = "Red"
  }
  case object Yellow extends Colours {
    override def value: String = "Yellow"
  }
  case object White extends Colours {
    override def value: String = "White"
  }
  case object Green extends Colours {
    override def value: String = "Green"
  }
  case object Blue extends Colours {
    override def value: String = "Blue"
  }

  def parse(
      char: Char
  ): Either[HanabiErrors, characteristics.Colours] = {
    char match {
      case 'r' => Right(Red)
      case 'y' => Right(Yellow)
      case 'w' => Right(White)
      case 'g' => Right(Green)
      case 'b' => Right(Blue)
      case _   => Left(ReadColourHint())
    }
  }
}
