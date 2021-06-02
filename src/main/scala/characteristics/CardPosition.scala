package characteristics

sealed trait CardPosition extends Ordered[CardPosition] {
  def toInt: Int
  override def compare(that: CardPosition): Int = this.toInt compare that.toInt
}

object CardPosition {
  case object First extends CardPosition {
    override def toInt: Int = 0
  }
  case object Second extends CardPosition {
    override def toInt: Int = 1
  }
  case object Third extends CardPosition {
    override def toInt: Int = 2
  }
  case object Fourth extends CardPosition {
    override def toInt: Int = 3
  }
  case object Fifth extends CardPosition {
    override def toInt: Int = 4
  }

  def parse(x: Int): CardPosition =
    x match {
      case 0 => First
      case 1 => Second
      case 2 => Third
      case 3 => Fourth
      case 4 => Fifth
    }
}
