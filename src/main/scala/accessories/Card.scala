package accessories

import characteristics.Colours

final case class Card(num: Int, colour: Colours) {
  override def toString: String = s"Card($num, $colour)"
}
