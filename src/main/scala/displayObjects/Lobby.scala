package displayObjects

import accessories.Card
import characteristics.Colours
import characteristics.Colours._

final case class Lobby(showroom: Map[Colours, Vector[Int]]) {

  def add(card: Card): Lobby = {
    println(s""" 
               |+ $card has been added to the lobby. 
               |+------------------------------------+
               |""".stripMargin)
    this.copy(showroom =
      showroom.updated(card.colour, showroom(card.colour) :+ card.num)
    )
  }

  def isCorrectCard(card: Card): Boolean = {
    val currentDisplayCards: Vector[Int] = showroom(card.colour)
    if (currentDisplayCards.contains(card.num)) {
      println(
        s"""
           |+ Wrong card! $card has already existed in the lobby. One life is lost!
           |+------------------------------------+
           |""".stripMargin
      )
      false
    } else {
      if (currentDisplayCards.isEmpty && card.num == 1) {
        println(s"+ Excellent choice! $card is the correct one.")
        true
      } else if (
        currentDisplayCards.nonEmpty && card.num - currentDisplayCards.last == 1
      ) {
        println(s"+ Excellent choice! $card is the correct one.")
        true
      } else {
        println(
          s"""
             |+ Wrong card! $card did not have the right number. One life is lost!
             |+------------------------------------+
             |""".stripMargin
        )
        false
      }
    }
  }
}

object Lobby {
  def apply(): Lobby =
    Lobby(
      Map(
        Red -> Vector[Int](),
        Yellow -> Vector[Int](),
        Green -> Vector[Int](),
        Blue -> Vector[Int](),
        White -> Vector[Int]()
      )
    )
}
