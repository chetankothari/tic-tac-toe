/*
  author: Chetan Kothari
  email: mail@chetankothari.in
  date: 01/09/2013
*/

package com.chetankothari.tictactoe

/*
  gamePlay is a list of moves each player has played at any point of time
  nextPlayer is the player who will be playing next
  winner declares the winner of the game it can be either Some(X), Some(O), Some(Empty) or None
  in case of Empty it means that the game was a Draw.
  None means that there is no winner yet and the game is not over.
*/

case class GamePlay(gamePlay: List[List[Player]] = List.fill(3)(List.fill(3)(new Empty)))

case class GameState(boardState: GamePlay = new GamePlay, nextPlayer: Player = new O, winner: Option[Player] = None) { 
  def displayBoardState = println(boardState.gamePlay.map(_.mkString(" | ")).mkString("\n---------\n"))

  def transpose(gamePlay: List[List[Player]]) = {
    val n = gamePlay.length
    (0 until n).map{ col =>
      (0 until n).map{ row =>
        gamePlay(row)(col)
      }.toList
    }.toList
  }

  def getDiagonals(gamePlay: List[List[Player]]) = {
    val n = gamePlay.length

    val leftDiagonal = (0 until n).flatMap{ col =>
      (0 until n).flatMap{ row =>
	if(row == col)
          Some(gamePlay(row)(col))
	else
	  None
      }
    }.toList

    val rightDiagonal = (0 until n).flatMap{ col =>
      (0 until n).flatMap{ row =>
	if(row + col == n - 1)
          Some(gamePlay(row)(col))
	else
	  None
      }
    }.toList

    List(leftDiagonal, rightDiagonal)

  }
  
  def getWinner(rows: List[List[Player]], newGamePlay: List[List[Player]]): Option[Player] = {

    val winner = rows.flatMap{ r =>
      r match {
        case List(a: O, b: O, c: O) => Some(new O)
        case List(a: X, b: X, c: X) => Some(new X)
        case _ => None
      }
    }

    val isStaleMate = !newGamePlay.flatten.contains(new Empty)
    if(winner.nonEmpty) {
      Some(winner.head)
    } else if(winner.isEmpty && isStaleMate) {
      Some(new Empty)
    } else {
      None
    }
  }

  def updateGameState(position: Int) = {
    val row = position / 3
    val col = (position - (row * 3))
    val player = nextPlayer
  
    val gamePlay = boardState.gamePlay.zipWithIndex.map{ r =>
      if(row == r._2) {
        r._1.zipWithIndex.map{ c =>
          if(col == c._2) {
            player
          } else {
            r._1(c._2)
          }
        }
      } else {
        r._1
      }
    }

    val newNextPlayer = player match {
      case p: X => new O
      case p: O => new X
    }
  
    val cols = transpose(gamePlay)

    val diagonals = getDiagonals(gamePlay)
  
    val rows = (gamePlay ++ cols ++ diagonals)

    val winner = getWinner(rows, gamePlay)

    new GameState(new GamePlay(gamePlay), newNextPlayer, winner)
  }

}
