////import GameUtils._
//import org.mockito.Mockito.when
//import org.scalatest.funsuite.AnyFunSuite
//import org.scalatestplus.mockito.MockitoSugar
//
//final class GameUtilsSpec extends AnyFunSuite with MockitoSugar {
//
//  val gameUtils = new GameUtils
//  test(
//    "askPlayerNames should return a Vector of size 2 with 2 String given two NumberOfPlayers."
//  ) {
//    val mockGameUtils: GameUtils = mock[GameUtils]
//    when(mockGameUtils.askName).thenReturn("PlayerName")
//    val twoPlayers = NumberOfPlayers.parse(2)
//    val actual = gameUtils.askPlayerNames(twoPlayers)
//    println(actual.mkString(","))
////    assert(actual.size == 2)
//  }
//}
