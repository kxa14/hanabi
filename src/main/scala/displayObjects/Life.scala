package displayObjects

final case class Life(count: Int) {
  def lose: Life = this.copy(count = count - 1)
}

object Life {
  def apply(): Life = Life(3)
}
