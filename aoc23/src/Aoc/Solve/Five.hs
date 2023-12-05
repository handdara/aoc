module Aoc.Solve.Five
  ( solveDay5,
  )
where

type Input = [String]

solutionPart1 :: Input -> String
solutionPart1 _ = "In Progress"

solutionPart2 :: Input -> String
solutionPart2 _ = "In Progress"

solveDay5 :: FilePath -> IO ()
solveDay5 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn "Input: "
  mapM_ putStrLn input'lines

  putStrLn $ "Solution to part 1: " ++ show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " ++ show (solutionPart2 input'lines)
