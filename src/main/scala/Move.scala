import CardNumber.CardNumberError
import CardNumber.CardNumberError.{InvalidCardNumbers, ReadCardNumberError}
import HanabiException.{
  InvalidCardNumbers,
  InvalidColour,
  InvalidHintMove,
  InvalidMove,
  InvalidYesNo,
  ReadCardNumberError
}

sealed trait Move extends Product with Serializable {
//  def decyptMove(player: Player)
}

//sealed trait Hint2 extends Move
//
//object Hint2 {
//  case object Colour extends Hint2
//  case object Number extends Hint2
//
//  def decyptMove(player: Player): Unit = {
//    println(s"""
//         |
//         | To hint (c)Colours or (n)Numbers?
//         |
//         |""".stripMargin)
//    val input = try {scala.io.StdIn.readChar().toLower} catch { case _: Throwable => Left(InvalidHintMove)}
//
//  def parse(char: Char) = char match {
//    case 'c' => Right(Colour)
//    case 'n' => Right(Number)
//  }
//}
//val input: Either[HanabiException, Colours] = try {Colours.parse(scala.io.StdIn.readChar().toLower)} catch { case _: Throwable => Left(InvalidHintMove)}

object Move {

  sealed trait Hint extends Move {
    def moreThanOneCards(): Either[HanabiException, YesNo]
  }
  object Hint {
    case object Colour extends Hint {
      def moreThanOneCards() =
        try {
          println(s"""
                     | Are there more than one card with the same colour?
                     | (y)Yes (n)No
                     |""".stripMargin)
          YesNo.parse(scala.io.StdIn.readChar().toLower)
        } catch { case _: Throwable => Left(InvalidYesNo()) }
    }

    case object Number extends Hint {
      override def moreThanOneCards() =
        try {
          println(s"""
                     | Are there more than one card with the same number?
                     | (y)Yes (n)No
                     |""".stripMargin)
          YesNo.parse(scala.io.StdIn.readChar().toLower)
        } catch { case _: Throwable => Left(InvalidYesNo()) }
    }

    /**
      *
      * @param gameState
      * @param giveHintPlayer
      * @param receiveHintPlayer
      * @return a hint in String type given by one player to the other
      */
    def decyptMove(
        gameState: GameState,
        giveHintPlayer: Player,
        receiveHintPlayer: Player
    ): Either[HanabiException, String] = {
      hintColourOrNumber() match {
        case Left(ex) =>
          println(ex.msg)
          hintColourOrNumber()
        case Right(hint) =>
          hint match {
            case Colour =>
              whatColour() match {
                case Left(ex) =>
                  println(ex.msg)
                  whatColour()
                case Right(validColour) =>
                  Colour.moreThanOneCards() match {
                    case Left(ex) =>
                      println(ex.msg)
                      Colour.moreThanOneCards()
                    case Right(yesOrNo) =>
                      if (yesOrNo == YesNo.No)
                        whichCard(gameState.players.size) match {
                          case Left(ex) =>
                            println(ex)
                            whichCard(gameState.players.size)
                          case Right(cardNumber) =>
                            Right(s"""
                         |+------------------------------------+
                         |+ 
                         |+ ${receiveHintPlayer.name}, ${giveHintPlayer.name} hints that 
                         |+ $cardNumber card is $validColour.
                         |+
                         |+------------------------------------+
                         |""".stripMargin)
                        }
                      else {
                        whichCards(gameState.players.size) match {
                          case Left(err) =>
                            println(err)
                            whichCards(gameState.players.size)
                          case Right(cardNums) =>
                            Right(s"""
                            |+------------------------------------+
                            |+ 
                            |+ ${receiveHintPlayer.name}, ${giveHintPlayer.name} hints that 
                            |+ Cards ${cardNums.mkString(",")} are $validColour.
                            |+
                            |+------------------------------------+
                            |""".stripMargin)
                        }
                      }
                  }
              }
            case Number =>
          }
      }
    }

    def parse(inputChar: Char): Either[HanabiException, Hint] =
      inputChar match {
        case 'c' => Right(Hint.Colour)
        case 'n' => Right(Hint.Number)
        case _   => Left(InvalidHintMove())
      }

    def hintColourOrNumber(): Either[HanabiException, Hint] =
      try {
        println(s"To hint (c)Colour or (n)Number?")
        Hint.parse(scala.io.StdIn.readChar().toLower)
      } catch { case _: Throwable => Left(InvalidHintMove()) }

    def whatColour(): Either[HanabiException, Colours] =
      try {
        println(s"""
                 | What colour?
                 | (r)Red (y)Yellow (w)White (g)Green (b)Blue                       |
                 |""".stripMargin)
        Colours.parse(scala.io.StdIn.readChar().toLower)
      } catch { case _: Throwable => Left(InvalidColour()) }

    def whichCard(numOfPlayers: Int): Either[CardNumberError, CardNumber] = {
      try {
        printCardOptions()
        CardNumber.parse(scala.io.StdIn.readInt())
      } catch {
        case ex: Throwable => Left(ReadCardNumberError(_))
      }

      def printCardOptions() =
        if (numOfPlayers <= 3)
          println(s"""
                   | Which card?
                   | (1)First (2)Second (3)Third (4)Fourth (5)Fifth
                   |""".stripMargin)
        else
          println(s"""
                   | Which card?
                   | (1)First (2)Second (3)Third (4)Fourth
                   |""".stripMargin)

    }

    def whichCards(
        numOfPlayers: Int
    ): Either[CardNumberError, Vector[CardNumber]] = {
      try {
        printCardOptions()
        CardNumber.parse(scala.io.StdIn.readLine())
      } catch {
        case _: Throwable => Left(ReadCardNumberError(_))
      }

      def printCardOptions() =
        if (numOfPlayers <= 3)
          println(s"""
                     | Which cards? Use comma(,) to split multiple numbers.
                     | Example input for two cards (Second and Fifth) with the same number/colour= 2,5
                     | 
                     | (1)First (2)Second (3)Third (4)Fourth (5)Fifth
                     |""".stripMargin)
        else
          println(s"""
                     | Which cards? Use comma(,) to split multiple numbers.
                     | Example input for two cards (Second and Fourth) with the same number/colour= 2,4
                     | 
                     | (1)First (2)Second (3)Third (4)Fourth
                     |""".stripMargin)

    }

//    def moreThanOneCards() =
//      try {
//        println(s"""
//                 | Are there more than one card with the same colour?
//                 | (y)Yes (n)No
//                 |""".stripMargin)
//        YesNo.parse(scala.io.StdIn.readChar().toLower)
//      } catch { case _: Throwable => Left(InvalidYesNo()) }

  }

  case object Play extends Move
  case object Discard extends Move

  def parse(inputChar: Char): Either[HanabiException, Move] = {
    inputChar.toLower match {
      case 'h' => Right(Hint)
      case 'p' => Right(Play)
      case 'd' => Right(Discard)
      case _   => Left(InvalidMove())
    }
  }

}
