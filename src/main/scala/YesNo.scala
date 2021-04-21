import HanabiException.InvalidYesNo

trait YesNo extends Product with Serializable {
//  def response: Either[]
}

object YesNo {
  final case object Yes extends YesNo
  final case object No extends YesNo

  def parse(char: Char) =
    char match {
      case 'y' => Right(Yes)
      case 'n' => Right(No)
      case _   => Left(InvalidYesNo())
    }
}
