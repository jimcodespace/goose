package game.model

object GooseState {
  val START_SPACE = 0
  val MIN_GAME_SPACE = 1
  val MAX_WINNING_GAME_SPACE = 63
  val BRIDGE_SPACE_POSITION = 4
  val BRIDGE_DESTINATION_POSITION = 12
  val GOOSE_SPACE_SET: Set[Int] = Set(5, 9, 14, 18, 23, 27)
  val MIN_DICE_VALUE = 1
  val MAX_DICE_VALUE = 6
  val SINGLE_GOOSE_REPLAY_TIMES = 2

  def getWinningMessage(playerName: String, dice1: Int, dice2: Int, initialPlayerSpace: Int, destinationSpace: Int): String =
    s"$playerName rolls $dice1, $dice2. $playerName moves from $initialPlayerSpace to $destinationSpace. $playerName Wins!!"

  def getBridgeMessage(playerName: String, dice1: Int, dice2: Int, initialPlayerSpace: Int): String =
    s"$playerName rolls $dice1, $dice2. $playerName moves from $initialPlayerSpace to The Bridge. $playerName jumps to $BRIDGE_DESTINATION_POSITION"

  def getSimpleMoveMessage(playerName: String, dice1: Int, dice2: Int, initialPlayerSpace: Int, destinationSpace: Int): String =
    s"$playerName rolls $dice1, $dice2. $playerName moves from $initialPlayerSpace to $destinationSpace"

  def getSingleGooseMessage(playerName: String, dice1: Int, dice2: Int, initialPlayerSpace: Int, intermediateDestinationSpace: Int, finalDestinationSpace: Int) =
    s"$playerName rolls $dice1, $dice2. $playerName moves from $initialPlayerSpace to $intermediateDestinationSpace, The Goose. $playerName moves again and goes to $finalDestinationSpace"

  def getMultipleGooseMessage(playerName: String, dice1: Int, dice2: Int, gooseMultipleDestinations: Seq[Int]): String = {
    val message = new StringBuilder(s"$playerName rolls $dice1, $dice2. $playerName moves from ${gooseMultipleDestinations(0)} to ${gooseMultipleDestinations(1)}, The Goose. ")
    (2 until gooseMultipleDestinations.size - 1).foreach { i => message.append(s"$playerName moves again and goes to ${gooseMultipleDestinations(i)}, The Goose. ") }
    message.append(s"$playerName moves again and goes to ${gooseMultipleDestinations(gooseMultipleDestinations.size - 1)}.")
    message.toString
  }

  def getPrankMessage(playerName: String, playerInitialState:Int, playerDestination: Int,
                      dice1: Int, dice2: Int, parknedPlayers: Seq[String]): String =
    s"$playerName rolls $dice1, $dice2. $playerName moves " +
      s"from $playerInitialState $playerDestination. On $playerDestination there are the following players: " +
      s"${parknedPlayers.mkString(",")}, who return to $playerInitialState"
}

case class GooseState(playerStateMap: Map[String, Int], hasNext: Boolean, gameMessage: String)

case class PlayerRoll(playerName: String, roll: (Int, Int))