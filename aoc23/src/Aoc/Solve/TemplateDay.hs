{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Aoc.Solve.X
  ( solveDayX,
    -- dbg
    Input,
    solutionPart1,
    solutionPart2,
  )
where

type Input = [String]

solutionPart1 :: Input -> String
solutionPart1 = const ("In Progress" :: String)

solutionPart2 :: Input -> String
solutionPart2 = const ("In Progress" :: String) 

solveDayX :: FilePath -> IO ()
solveDayX input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn "Input: "
  mapM_ putStrLn input'lines

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
