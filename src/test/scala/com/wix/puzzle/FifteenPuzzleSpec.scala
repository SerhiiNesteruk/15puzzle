package com.wix.puzzle

import org.scalatest.{FlatSpec, Matchers}

class FifteenPuzzleSpec extends FlatSpec with Matchers {
  import FifteenPuzzle._
  val dimension = 4
  val puzzle = new FifteenPuzzle(dimension)

  "FifteenPuzzle" should "correct initialize board" in {
    val initState = puzzle.init
    val allValues = (holeValue :: (1 until dimension*dimension).toList).toSet
    initState.board should contain theSameElementsAs allValues
  }

  val testBoard = Vector(
    1, 2, 3, 4,
    5, 6, holeValue, 8,
    9, 10, 11, 12,
    13, 14, 15, 7
  )
  val testHole = Point(2, 1)
  val testState = PlayState(testBoard, testHole)

  it should "correctly move up if possible" in {
    val newBoard = puzzle.up(testState)
    newBoard.board should contain theSameElementsInOrderAs Vector(
      1, 2, holeValue, 4,
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
      9, 10, holeValue, 12,
      13, 14, 15, 7
    )
    newBoard.hole shouldBe testHole.copy(y = testHole.y + 1)
  }

  it should "correctly move left if possible" in {
    val newBoard = puzzle.left(testState)
    newBoard.board should contain theSameElementsInOrderAs Vector(
      1, 2, 3, 4,
      5, holeValue, 6, 8,
      9, 10, 11, 12,
      13, 14, 15, 7
    )
    newBoard.hole shouldBe testHole.copy(x = testHole.x - 1)
  }

  it should "correctly move right if possible" in {
    val newBoard = puzzle.right(testState)
    newBoard.board should contain theSameElementsInOrderAs Vector(
      1, 2, 3, 4,
      5, 6, 8, holeValue,
      9, 10, 11, 12,
      13, 14, 15, 7
    )
    newBoard.hole shouldBe testHole.copy(x = testHole.x + 1)
  }

  it should "not change state if move up is not possible" in {
    val cornerBoard = Vector(
      1, 2, holeValue, 4,
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
      13, 14, holeValue, 7
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
      holeValue, 10, 11, 12,
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
      12, 10, 11, holeValue,
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
      13, 14, 15, holeValue
    )
    val hole = Point(3, 3)
    val winState = PlayState(board, hole)
    puzzle.checkWin(winState) shouldBe true

    val notWinBoard = Vector(
      holeValue, 1, 2, 3,
      4, 5, 6, 7,
      8, 9, 10, 11,
      12, 13, 14, 15
    )
    val notWinHole = Point(0, 0)
    val notWinState = PlayState(notWinBoard, notWinHole)
    puzzle.checkWin(notWinState) shouldBe false
  }

  it should "check if board is solvable for odd dimensions" in {
    val boardSolvable = Vector(
      1, 2, holeValue,
      4, 5, 3,
      7, 8, 6
    )

    val boardImpossible = Vector(
      1, 2, 3,
      4, 5, 6,
      8, 7, holeValue
    )

    val winBoard = Vector(
      1, 2, 3,
      4, 5, 6,
      7, 8, holeValue
    )

    isSolvable(boardSolvable, 3) shouldBe true
    isSolvable(boardImpossible, 3) shouldBe false
    isSolvable(winBoard, 3) shouldBe true
  }

  it should "check if board is solvable for even dimensions" in {
    val boardSolvable = Vector(
      1, 2, 3, 4,
      5, 6, 7, holeValue,
      9, 10, 11, 8,
      13, 14, 15, 12
    )

    val boardImpossible = Vector(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 15, 14, holeValue
    )

    val winBoard = Vector(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, holeValue
    )

    isSolvable(boardSolvable, 3) shouldBe true
    isSolvable(boardImpossible, 3) shouldBe false
    isSolvable(winBoard, 3) shouldBe true
  }

  it should "always generate solvable board" in {
    val numOfGenerations = 10
    (0 to numOfGenerations).foreach {_ =>
      val newGame = puzzle.init
      isSolvable(newGame.board, dimension) shouldBe true
    }
  }
}
