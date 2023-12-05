module Aoc.Solve.Four
  ( solveDay4,
  )
where

type Input = [String]

solutionPart1 :: Input -> String
solutionPart1 _ = "In Progress"

solutionPart2 :: Input -> String
solutionPart2 _ = "In Progress"

solveDay4 :: FilePath -> IO ()
solveDay4 input = do
  input'string <- readFile input

  let puzzleInput = lines input'string :: Input

  let answerPart1 = solutionPart1 puzzleInput

  putStrLn $ "Solution to part 1: " ++ show answerPart1

  let answerPart2 = solutionPart2 puzzleInput

  putStrLn $ "Solution to part 2: " ++ show answerPart2
