package action

sealed trait Move extends Product with Serializable

object Move {

  final case object Hint extends Move
  final case object Play extends Move
  final case object Discard extends Move

  def parse(char: Char): Move =
    char.toLower match {
      case 'h' => Hint
      case 'p' => Play
      case 'd' => Discard
    }
}
