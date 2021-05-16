package action
import errors.HanabiErrors.OutOfHintTokens
import org.scalamock.scalatest.MockFactory
import org.scalatest.funspec.AnyFunSpec

class MoveSpec extends AnyFunSpec with MockFactory {

  describe("Given HintTokens's count is zero") {
    it ("should return OutOfHintTokens error") {
      assert(Move.parseNoHint('h') == Left(OutOfHintTokens()))
    }
  }
}


