package displayObjects

final case class Life(count: Int) {
  def lose: Life =
    if (count <= 0 || count == -99) this else this.copy(count = count - 1)
}
