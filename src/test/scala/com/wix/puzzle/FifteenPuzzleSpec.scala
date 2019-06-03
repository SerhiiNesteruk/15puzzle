package com.wix.puzzle

import org.scalatest.{FlatSpec, Matchers}

class FifteenPuzzleSpec extends FlatSpec with Matchers {
  val dimension = 4
  val puzzle = new FifteenPuzzle(dimension)

  "FifteenPuzzle" should "correct initialize board" in {
    val initState = puzzle.init
    val allValues = (PlayState.holeValue :: (1 until dimension*dimension).toList).toSet
    initState.board should contain theSameElementsAs allValues
  }

  val testBoard = Vector(
    1, 2, 3, 4,
    5, 6, PlayState.holeValue, 8,
    9, 10, 11, 12,
    13, 14, 15, 7
  )
  val testHole = Point(2, 1)
  val testState = PlayState(testBoard, testHole)

  it should "correctly move up if possible" in {
    val newBoard = puzzle.up(testState)
    newBoard.board should contain theSameElementsInOrderAs Vector(
      1, 2, PlayState.holeValue, 4,
      5, 6, 3, 8,
      9, 10, 11, 12,
      13, 14, 15, 7
    )
    newBoard.hole shouldBe testHole.copy(y = testHole.y - 1)
  }

  it should "correctly move down if possible" in {
    val newBoard = puzzle.down(testState)
    newBoard.board should contain theSameElementsInOrderAs Vector(
      1, 2, 3, 4,
      5, 6, 11, 8,
      9, 10, PlayState.holeValue, 12,
      13, 14, 15, 7
    )
    newBoard.hole shouldBe testHole.copy(y = testHole.y + 1)
  }

  it should "correctly move left if possible" in {
    val newBoard = puzzle.left(testState)
    newBoard.board should contain theSameElementsInOrderAs Vector(
      1, 2, 3, 4,
      5, PlayState.holeValue, 6, 8,
      9, 10, 11, 12,
      13, 14, 15, 7
    )
    newBoard.hole shouldBe testHole.copy(x = testHole.x - 1)
  }

  it should "correctly move right if possible" in {
    val newBoard = puzzle.right(testState)
    newBoard.board should contain theSameElementsInOrderAs Vector(
      1, 2, 3, 4,
      5, 6, 8, PlayState.holeValue,
      9, 10, 11, 12,
      13, 14, 15, 7
    )
    newBoard.hole shouldBe testHole.copy(x = testHole.x + 1)
  }

  it should "not change state if move up is not possible" in {
    val cornerBoard = Vector(
      1, 2, PlayState.holeValue, 4,
      5, 6, 3, 8,
      9, 10, 11, 12,
      13, 14, 15, 7
    )
    val conrnerHole = Point(2, 0)
    val oldState = PlayState(cornerBoard, conrnerHole)

    val newState = puzzle.up(oldState)
    newState shouldBe oldState
  }

  it should "not change state if move down is not possible" in {
    val cornerBoard = Vector(
      1, 2, 15, 4,
      5, 6, 3, 8,
      9, 10, 11, 12,
      13, 14, PlayState.holeValue, 7
    )
    val conrnerHole = Point(2, 3)
    val oldState = PlayState(cornerBoard, conrnerHole)

    val newState = puzzle.down(oldState)

    newState shouldBe oldState
  }

  it should "not change state if move left is not possible" in {
    val cornerBoard = Vector(
      1, 2, 15, 4,
      5, 6, 3, 8,
      PlayState.holeValue, 10, 11, 12,
      13, 14, 9, 7
    )
    val conrnerHole = Point(0, 2)
    val oldState = PlayState(cornerBoard, conrnerHole)

    val newState = puzzle.left(oldState)

    newState shouldBe oldState
  }

  it should "not change state if move right is not possible" in {
    val cornerBoard = Vector(
      1, 2, 15, 4,
      5, 6, 3, 8,
      12, 10, 11, PlayState.holeValue,
      13, 14, 9, 7
    )
    val conrnerHole = Point(3, 2)
    val oldState = PlayState(cornerBoard, conrnerHole)

    val newState = puzzle.right(oldState)

    newState shouldBe oldState
  }

  it should "check win condition" in {
    val board = Vector(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, PlayState.holeValue
    )
    val hole = Point(3, 3)
    val winState = PlayState(board, hole)
    puzzle.checkWin(winState) shouldBe true

    val notWinBoard = Vector(
      PlayState.holeValue, 1, 2, 3,
      4, 5, 6, 7,
      8, 9, 10, 11,
      12, 13, 14, 15
    )
    val notWinHole = Point(0, 0)
    val notWinState = PlayState(notWinBoard, notWinHole)
    puzzle.checkWin(notWinState) shouldBe false
  }
}
