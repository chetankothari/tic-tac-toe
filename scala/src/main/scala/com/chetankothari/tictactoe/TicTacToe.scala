/*
  author: Chetan Kothari
  email: mail@chetankothari.in
  date: 01/09/2013
*/

package com.chetankothari.tictactoe

object TicTacToe extends App {

  def isPositionAvailable(gameState: GameState, position: Int) = {
    (gameState.boardState.gamePlay.flatten.toList)(position) == new Empty
  }

  def getPositionFromUser(message: String): Int = {
    println(message)
    try {
      readInt
    } catch {
      case e: java.lang.NumberFormatException => {
        println("Only Integer values allowed")
        getPositionFromUser(message)
      }
    }
  }
  
  def getNextMove(gameState: GameState): Int = {
    val position = gameState.nextPlayer match {
      case player: O => getPositionFromUser("O's turn: ")
      case player: X => getPositionFromUser("x's turn: ")
    }
    position match {
      case pos: Int if(pos >= 1 && pos <= 9) => {
        if(isPositionAvailable(gameState, pos - 1)) {
          pos - 1
        } else {
          println("Position already marked, please chose another position")
          getNextMove(gameState)
        }
      }
      case _ => {
        println("Enter value between 1 to 9")
        getNextMove(gameState)
      }
    }
  }

  def play(gameState: GameState) {
    gameState.displayBoardState
    val position = getNextMove(gameState)
    val updatedGameState = gameState.updateGameState(position)
    updatedGameState.winner match {
      case Some(winner: O) => { println("Player O Wins!"); updatedGameState.displayBoardState }
      case Some(winner: X) => { println("Player X Wins!"); updatedGameState.displayBoardState }
      case Some(winner: Empty) => { println("Game was a Draw"); updatedGameState.displayBoardState }
      case None => play(updatedGameState)
    }
  }

  val gameState = new GameState

  play(gameState)
} 
