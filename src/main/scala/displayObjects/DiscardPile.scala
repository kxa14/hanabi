package displayObjects

import accessories.Card
import characteristics.Colours
import characteristics.Colours.{Blue, Green, Red, White, Yellow}

final case class DiscardPile(showroom: Map[Colours, Vector[Int]]) {

  def discard(card: Card): DiscardPile =
    this.copy(showroom =
      showroom.updated(card.colour, showroom(card.colour) :+ card.num)
    )
}

object DiscardPile {
  def apply(): DiscardPile =
    DiscardPile(
      Map(
        Red -> Vector[Int](),
        Yellow -> Vector[Int](),
        Green -> Vector[Int](),
        Blue -> Vector[Int](),
        White -> Vector[Int]()
      )
    )
}
