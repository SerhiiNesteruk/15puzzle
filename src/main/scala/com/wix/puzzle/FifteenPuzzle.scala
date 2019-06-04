package com.wix.puzzle

import scala.util.Random

sealed trait GameState
case class PlayState(board: Vector[Int], hole: Point) extends GameState
case object TerminateState extends GameState

case class Point(x: Int, y: Int)

class FifteenPuzzle(dimension: Int = 4) {

  import FifteenPuzzle._

  def init: PlayState = {
    val randomBoard = Random.shuffle((1 until dimension * dimension).toVector) ++ Vector(holeValue)
    val correctBoard = if (isSolvable(randomBoard, dimension)) randomBoard else fixImpossibleBorad(randomBoard)
    PlayState(correctBoard, Point(dimension - 1, dimension - 1))
  }

  private def fixImpossibleBorad(board: Vector[Int]) = {
    val arr = board.toArray
    val tmp = board(0)
    arr(0) = board(1)
    arr(1) = tmp
    arr.toVector
  }

  def up(playState: PlayState): PlayState = {
    if (playState.hole.y == 0) playState
    else swap(playState, playState.hole.copy(y = playState.hole.y - 1))
  }

  def down(playState: PlayState): PlayState = {
    if (playState.hole.y == dimension - 1) playState
    else swap(playState, playState.hole.copy(y = playState.hole.y + 1))
  }

  def left(playState: PlayState): PlayState = {
    if (playState.hole.x == 0) playState
    else swap(playState, playState.hole.copy(x = playState.hole.x - 1))
  }

  def right(playState: PlayState): PlayState = {
    if (playState.hole.x == dimension - 1) playState
    else swap(playState, playState.hole.copy(x = playState.hole.x + 1))
  }

  def checkWin(playState: PlayState): Boolean = {
    def checkWin(list: List[Int], prev: Int): Boolean = {
      list match {
        case h :: Nil => h == holeValue
        case h :: t if h == prev + 1 => checkWin(t, h)
        case _ => false
      }
    }

    checkWin(playState.board.toList, 0)
  }

  private def swap(playState: PlayState, point: Point): PlayState = {
    val tmp = playState.board(playState.hole.y * dimension + playState.hole.x)
    val st1 = playState.board.updated(playState.hole.y * dimension + playState.hole.x, playState.board(point.y * dimension + point.x))
    val newState = st1.updated(point.y * dimension + point.x, tmp)
    PlayState(newState, point)
  }

  def prettyPrint(gameState: PlayState): Unit = {
    val maxLen = dimension * dimension.toString.length

    gameState.board.zipWithIndex.foreach { case (value, idx) =>
      val displayValue = if (value == holeValue) "_" else value.toString
      val spaces = maxLen - displayValue.length - 1
      print(displayValue + " " * spaces)
      if ((idx + 1) % dimension == 0) println()
    }

    println()
  }

}

object FifteenPuzzle {
  val holeValue: Int = -100

  def isSolvable(board: Vector[Int], dimension: Int): Boolean = {
    if (dimension % 2 == 0) isEvenSolvable(board: Vector[Int])
    else isOddSolvable(board: Vector[Int])
  }

  private def isEvenSolvable(board: Vector[Int]): Boolean = {
    //we assume that hole is always at bottom
    numOfInversions(board) % 2 == 0
  }

  private def isOddSolvable(board: Vector[Int]): Boolean = {
    numOfInversions(board) % 2 == 0
  }

  private def numOfInversions(board: Vector[Int]): Int = {
    var inversions = 0
    for(
      i <- board.indices if board(i) != holeValue;
      j <- i until board.size if board(j) != holeValue
    ) {
      if (board(i) > board(j)) {
        inversions = inversions + 1
      }
    }
    inversions
  }
}
