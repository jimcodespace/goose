package game.service

import game.model.{GooseState, PlayerRoll}

import scala.collection.mutable.ListBuffer
import scala.util.Random

class GooseService() {
  import game.model.GooseState._

  def getInitialGooseState(players: Set[String]): GooseState = GooseState(playerStateMap = players.map(_ -> START_SPACE).toMap, hasNext = true, gameMessage = "")

  def rollRandomDices(): (Int, Int) = (Random.between(MIN_DICE_VALUE, MAX_DICE_VALUE + 1), Random.between(MIN_DICE_VALUE, MAX_DICE_VALUE + 1))

  def move(currentGameState: GooseState, playerRoll: PlayerRoll): GooseState = {
    val (dice1, dice2) = (playerRoll.roll._1, playerRoll.roll._2)
    val diceSum = dice1 + dice2
    val initialPlayerState = currentGameState.playerStateMap(playerRoll.playerName)


    initialPlayerState + diceSum match {
      case BRIDGE_SPACE_POSITION =>
        GooseState(
          playerStateMap = currentGameState.playerStateMap + (playerRoll.playerName -> BRIDGE_DESTINATION_POSITION),
          hasNext = true,
          gameMessage = getBridgeMessage(playerRoll.playerName, dice1, dice2, initialPlayerState)
        )

      case state if GOOSE_SPACE_SET.contains(state) => handleGooseState(currentGameState, playerRoll)

      case MAX_WINNING_GAME_SPACE =>
        GooseState(
          playerStateMap = currentGameState.playerStateMap + (playerRoll.playerName -> MAX_WINNING_GAME_SPACE),
          hasNext = false,
          getWinningMessage(playerRoll.playerName, dice1, dice2, initialPlayerState, MAX_WINNING_GAME_SPACE)
        )

      case state if currentGameState.playerStateMap.values.toSeq.contains(state) =>
        val prankedPlayers = currentGameState.playerStateMap.filter(_._2 == state).keys.toSeq
        GooseState(
          playerStateMap = currentGameState.playerStateMap + (playerRoll.playerName -> state),
          hasNext = true,
          getPrankMessage(playerRoll.playerName, initialPlayerState, state, dice1, dice2, prankedPlayers)
        )

      case state =>
        GooseState(
          playerStateMap = currentGameState.playerStateMap + (playerRoll.playerName -> (initialPlayerState + diceSum)),
          hasNext = true,
          getSimpleMoveMessage(playerRoll.playerName, dice1, dice2, initialPlayerState, state)
        )
    }
  }

  private def handleGooseState(currentGameState: GooseState, playerRoll: PlayerRoll): GooseState = {
    val initialState = currentGameState.playerStateMap(playerRoll.playerName)
    val gooseLoop: scala.collection.mutable.ListBuffer[Int] = ListBuffer(initialState)
    val diceSum = playerRoll.roll._1 + playerRoll.roll._2

    gooseLoop += (initialState + diceSum)

    while (GOOSE_SPACE_SET.contains(gooseLoop.last)) {
      gooseLoop += (gooseLoop.last + diceSum)
    }

    GooseState(
      playerStateMap = currentGameState.playerStateMap + (playerRoll.playerName -> gooseLoop.last),
      hasNext = true,
      if (gooseLoop.size > SINGLE_GOOSE_REPLAY_TIMES) {
        getMultipleGooseMessage(playerRoll.playerName, playerRoll.roll._1, playerRoll.roll._2, gooseLoop.toSeq)
      } else {
        getSingleGooseMessage(playerRoll.playerName, playerRoll.roll._1, playerRoll.roll._2, initialState, initialState + diceSum, gooseLoop.last)
      }
    )

  }


}
