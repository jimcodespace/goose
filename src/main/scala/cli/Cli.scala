package cli

import game.model.{GooseState, PlayerRoll}
import game.service.GooseService

import scala.util.matching.Regex
import scala.util.control.Breaks


object Cli extends App {
  val MIN_GOOSE_GAME_PLAYERS = 2
  val PLAYER_ADDED = "players: "
  val DUPLICATED_PLAYER = ": already existing player"
  val START_COMMAND = "start"
  val EXIT_COMMAND = "exit"
  val addPlayerExpectedPattern: Regex = "add player ([A-Za-z0-9]+)".r
  val movePlayerWithDiceGivenExpectedPattern: Regex = "move ([A-Za-z0-9]+) ([1-6]), ([1-6])".r
  val movePlayerExpectedPattern: Regex = "move ([A-Za-z0-9]+)".r

  val players: scala.collection.mutable.Set[String] = scala.collection.mutable.Set.empty

  Breaks.breakable {
    do {
      scala.io.StdIn.readLine() match {
        case addPlayerExpectedPattern(playerName) => handleAddPlayerCommand(playerName)
        case command if command == START_COMMAND =>
          handleStartGameCommand()
          Breaks.break()
        case command if command == EXIT_COMMAND => handleExitCommand()
        case _ => handleInvalidAddPlayerCommand()
      }
    } while(true)
  }

  val gooseService = new GooseService()
  var gooseState: GooseState = gooseService.getInitialGooseState(players.toSet)

  do {
    scala.io.StdIn.readLine() match {
      case movePlayerWithDiceGivenExpectedPattern(playerName, dice1, dice2) =>
        gooseState = gooseService.move(gooseState, PlayerRoll(playerName, (dice1.toInt, dice2.toInt)))
        handleGameStateChange(gooseState)
      case movePlayerExpectedPattern(playerName) =>
        gooseState = gooseService.move(gooseState, PlayerRoll(playerName, gooseService.rollRandomDices()))
        handleGameStateChange(gooseState)
      case command if command == EXIT_COMMAND => handleExitCommand()
      case _ => handleInvalidMoveCommand()
    }

  } while (gooseState.hasNext)


  def handleAddPlayerCommand(playerName: String): Unit = {
    if (!players.contains(playerName)) {
      players += playerName
      println(PLAYER_ADDED + players.mkString(", "))
    } else {
      println(playerName + DUPLICATED_PLAYER)
    }
  }

  def handleStartGameCommand(): Unit = {
    if (players.size < MIN_GOOSE_GAME_PLAYERS) {
      println("Game cannot be started")
      println("Exiting...")
      System.exit(1)
    } else {
      println("Starting Goose Game...")
    }
  }



  def handleExitCommand(): Unit = {
    println("Exiting...")
    System.exit(1)
  }

  def handleInvalidMoveCommand(): Unit = println("""Invalid command. Expected:  "move [player name] [dice1] [dice2]" or "move [player name]" for random dice rolling from the game.""")

  def handleInvalidAddPlayerCommand():Unit = println("""Invalid command. Expected: "add player [player name]" or "start" to start the game.""")

  def handleGameStateChange(gooseState: GooseState): Unit = println(gooseState.gameMessage)
}