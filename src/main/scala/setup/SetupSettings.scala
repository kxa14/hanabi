package setup

trait NumberOfPlayers[F[_]] {
  def run: F[Int]
}

trait PlayersNames[F[_]] {
  def run(numOfPlayers: Int): F[Vector[String]]
}
