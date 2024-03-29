package displayObjects

final case class HintTokens(count: Int) {
  def lose: HintTokens = if (count <= 0) this else this.copy(count = count - 1)
  def gain: HintTokens = if (count >= 8) this else this.copy(count = count + 1)
}
