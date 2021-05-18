package util

import accessories.{CardDeck, Player}
import action.Move
import cats.effect.IO
import characteristics.{CardPosition, NumberOfPlayers}
import displayObjects.{GameState, HintTokens}
import errors.HanabiErrors
import errors.HanabiErrors._
import monocle.macros.syntax.lens._

import scala.annotation.tailrec

object GameUtils {

  def askNumberOfPlayers: NumberOfPlayers =
    try {
      println(
        s"How many number of players?"
      ) // TODO: side effect, not referential transparency.
      NumberOfPlayers.parse(scala.io.StdIn.readInt()) match {
        case Left(err) =>
          println(err)
          askNumberOfPlayers
        case Right(numberOfPlayers) => numberOfPlayers
      }
    } catch {
      case _: Throwable =>
        println(ReadIntErrors())
        askNumberOfPlayers
    }

  def askPlayerNames(
      num: NumberOfPlayers
  ): Vector[String] = {
    num match {
      case NumberOfPlayers.Two => Vector(askPlayerName, askPlayerName)
      case NumberOfPlayers.Three =>
        Vector(askPlayerName, askPlayerName, askPlayerName)
      case NumberOfPlayers.Four =>
        Vector(askPlayerName, askPlayerName, askPlayerName, askPlayerName)
      case NumberOfPlayers.Five =>
        Vector(
          askPlayerName,
          askPlayerName,
          askPlayerName,
          askPlayerName,
          askPlayerName
        )
    }
  }

  @tailrec
  private def askPlayerName: String =
    try {
      println(s"What is the name of the player?")
      scala.io.StdIn.readLine()
    } catch {
      case _: Throwable =>
        println(ReadStringErrors())
        askPlayerName
    }

  def askPlayerMove(
      player: Player,
      hintTokens: HintTokens,
      choice: => Char
  ): Move = {
    if (hintTokens.count > 0) {
      try {
        println(
          s"""${player.name}, make a choice: (h)hint, (p)play, (d)discard""".stripMargin
        )
        Move.parse(choice) match {
          case Left(err) =>
            println(err)
            askPlayerMove(player, hintTokens, choice)
          case Right(move) => move
        }
      } catch {
        case _: Throwable =>
          println(ReadCharErrors())
          askPlayerMove(player, hintTokens, choice)
      }
    } else {
      try {
        println(
          s"""${player.name}, make a choice: (p)play, (d)discard""".stripMargin
        )
        Move.parseNoHint(choice) match {
          case Left(err) =>
            println(err)
            askPlayerMove(player, hintTokens, choice)
          case Right(move) => move
        }
      } catch {
        case _: Throwable =>
          println(ReadCharErrors())
          askPlayerMove(player, hintTokens, choice)
      }
    }
  }

  def whichPlayerToHint(gameState: GameState, hintGiver: Player): Player = {
    val otherPlayers = gameState.players.filterNot(_ == hintGiver)
    val playerOptionsToString: String =
      otherPlayers.map(p => s"${p.id})${p.name} ").foldLeft("")(_ + _)

    def validateGivenPlayerId(
        chosenPlayerNum: Int
    ): Either[HanabiErrors, Player] =
      Either.cond(
        otherPlayers.map(_.id).contains(chosenPlayerNum),
        otherPlayers.filter(_.id == chosenPlayerNum).head,
        InvalidPlayerID(chosenPlayerNum)
      )

    try {
      println(s"""
           |${hintGiver.name}, who do you want to hint?
           |$playerOptionsToString
           |""".stripMargin)
      validateGivenPlayerId(scala.io.StdIn.readInt()) match {
        case Left(err) =>
          println(s"$err")
          whichPlayerToHint(gameState, hintGiver)
        case Right(player) => player
      }
    } catch {
      case _: Throwable =>
        println(ReadIntErrors())
        whichPlayerToHint(gameState, hintGiver)
    }
  }

  def whichCards(
      multipleCardOptions: MultipleCardOptions
  ): List[CardPosition] =
    try {
      multipleCardOptions.list(scala.io.StdIn.readLine()) match {
        case Left(err) =>
          println(err)
          whichCards(multipleCardOptions)
        case Right(cardPositions) => cardPositions
      }
    } catch {
      case _: Throwable =>
        println(ReadStringErrors())
        whichCards(multipleCardOptions)
    }

  def whichCard(cardOptions: SingleCardOptions): CardPosition =
    try {
      cardOptions.list(scala.io.StdIn.readInt()) match {
        case Left(err) =>
          println(err)
          whichCard(cardOptions)
        case Right(cardPosition) => cardPosition
      }
    } catch {
      case _: Throwable =>
        println(ReadIntErrors())
        whichCard(cardOptions)
    }

  def parseYesNo(char: Char): Either[HanabiErrors, Boolean] =
    char match {
      case 'y' => Right(true)
      case 'n' => Right(false)
      case _   => Left(InvalidYesNo())
    }

  /** For both Play's and Discard's move, each player's hand will need to be updated (dropping and/or adding one card) and
    * CardDeck will need to drop the first card if there is any.
    *
    * @param gameState
    * @param player
    * @param cardPosition
    * @return
    */
  def updatePlayerAndCardDeck(
      gameState: GameState,
      player: Player,
      cardPosition: CardPosition
  ): GameState =
    gameState
      .lens(_.players)
      .modify(
        _.updated(
          player.id,
          GameUtils.updatePlayerHand(player, gameState.cardDeck, cardPosition)
        )
      )
      .lens(_.cardDeck.cards)
      .modify(_.drop(1))

  /** Drops the selected card, then, only if there is still card in the CardDeck, adds one card into the player's hand.
    *
    * @param player
    * @param cardDeck
    * @param cardPosition
    * @return
    */
  def updatePlayerHand(
      player: Player,
      cardDeck: CardDeck,
      cardPosition: CardPosition
  ): Player = {
    val playerWithOneCardLess = player
      .lens(_.hand)
      .modify(
        _.zipWithIndex
          .filterNot(_._2 == cardPosition.toInt)
          .map(_._1)
      )
    cardDeck.cards.headOption
      .map(card => playerWithOneCardLess.lens(_.hand).modify(_ :+ card))
      .getOrElse(playerWithOneCardLess)
  }

  // TODO: keep it here for now until start using IO
  private def printLine(msg: String): IO[Unit] =
    IO {
      println(msg)
    }

  // TODO: keep it here for now until start using IO
  private def askStringInput: IO[String] =
    IO {
      scala.io.StdIn.readLine()
    }
}
