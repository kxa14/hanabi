package characteristics

sealed trait Clue extends Product with Serializable {
  def name: String
}

object Clue {
  final case class Colour(name: String = "colour") extends Clue
  object Colour {
    def parse(char: Char): String =
      char.toLower match {
        case 'r' => "Red"
        case 'y' => "Yellow"
        case 'w' => "White"
        case 'g' => "Green"
        case 'b' => "Blue"
      }
  }

  final case class Number(name: String = "number") extends Clue
  object Number {

    def parse(num: Int): String =
      num match {
        case 1 => "One"
        case 2 => "Two"
        case 3 => "Three"
        case 4 => "Four"
        case 5 => "Five"
      }
  }
}
