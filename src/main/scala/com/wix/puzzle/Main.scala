package com.wix.puzzle

import cats.effect.IO

object Main extends App {
  val game = for {
    _ <- IO(println("Greetings traveler! Use w,a,s,d to swap or quit to quit the game. Good Luck!"))

    puzzle <- IO(new FifteenPuzzle())
    initState <- IO(puzzle.init)
    _ <- gameLoop(puzzle, initState)
  } yield ()

  game.unsafeRunSync()


  def gameLoop(puzzle: FifteenPuzzle, gameState: PlayState): IO[Unit] = {
    for {
      _ <- IO(puzzle.prettyPrint(gameState))
      newState <- makeProgress(gameState, puzzle)
      isWinner <- IO(puzzle.checkWin(gameState))
      _ <- if (isWinner) IO(println("You win this game")) else newState match {
        case TerminateState => IO(println("Please come again!"))
        case playState: PlayState => gameLoop(puzzle, playState)
      }
    } yield ()

  }

  def makeProgress(state: PlayState, puzzle: FifteenPuzzle) = {
    for {
      in <- IO(Console.in.readLine())
      newState <- IO(nextMove(in, state, puzzle))
    } yield newState
  }

  def nextMove(in: String, playState: PlayState, puzzle: FifteenPuzzle): GameState = {
    in match {
      case "a" => puzzle.left(playState)
      case "w" => puzzle.up(playState)
      case "s" => puzzle.down(playState)
      case "d" => puzzle.right(playState)
      case "quit" => TerminateState
      case _ => playState
    }
  }
}