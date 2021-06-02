package characteristics

sealed trait Colours extends Product with Serializable

object Colours {
  case object Red extends Colours

  case object Yellow extends Colours

  case object White extends Colours

  case object Green extends Colours

  case object Blue extends Colours
}
