//package displayObjects
//import characteristics.Colours.Red
//import org.scalatest.funspec.AnyFunSpec
//import accessories.Card
//
//class LobbySpec extends AnyFunSpec {
//
//  describe("isCorrectCard") {
//
//    describe("When given a Card that has already existed") {
//      val stubCard = Card(1, Red)
//      val mockLobby = Lobby(Map(Red -> Vector(1)))
//      it("should return false") {
//        assert(!mockLobby.isCorrectCard(stubCard))
//      }
//    }
//
//    describe("When the lobby of a given colour is empty") {
//      val mockEmptyLobby = Lobby()
//      describe("Given a card with number not equal to 1") {
//        it("should return false") {
//          assert(!mockEmptyLobby.isCorrectCard(Card(2, Red)))
//          assert(!mockEmptyLobby.isCorrectCard(Card(3, Red)))
//          assert(!mockEmptyLobby.isCorrectCard(Card(4, Red)))
//          assert(!mockEmptyLobby.isCorrectCard(Card(5, Red)))
//        }
//      }
//      describe("Given a card with number equal to 1") {
//        it("should return true") {
//          assert(mockEmptyLobby.isCorrectCard(Card(1, Red)))
//        }
//      }
//    }
//
//    describe("When the lobby of a given colour is non-empty") {
//      val mockNonEmptyLobby = Lobby(Map(Red -> Vector(1)))
//      describe(
//        "Given a card with number greater than the last number of the list of number by 1"
//      ) {
//        it("should return true") {
//          assert(mockNonEmptyLobby.isCorrectCard(Card(2, Red)))
//        }
//      }
//      describe(
//        "Given a card with number greater than the last number of list of number by more than 1"
//      ) {
//        it("should return false") {
//          assert(!mockNonEmptyLobby.isCorrectCard(Card(3, Red)))
//          assert(!mockNonEmptyLobby.isCorrectCard(Card(4, Red)))
//          assert(!mockNonEmptyLobby.isCorrectCard(Card(5, Red)))
//        }
//      }
//      describe(
//        "Given a card with number equal to the last number of list of number"
//      ) {
//        it("should return false") {
//          assert(!mockNonEmptyLobby.isCorrectCard(Card(1, Red)))
//        }
//      }
//    }
//  }
//
//  describe("apply()") {
//    val mockLobby = Lobby()
//    it("should instantiate an empty Vector of type Int for each colour") {
//      assert(mockLobby.showroom(Red).isEmpty)
//    }
//  }
//}
