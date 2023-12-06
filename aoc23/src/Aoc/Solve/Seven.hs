{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Aoc.Solve.Seven
  ( solveDay7,
    -- dbg
    Input,
  )
where

type Input = [String]

solutionPart1 = const ("In Progress" :: String)

solutionPart2 = const ("In Progress" :: String) 

solveDay7 :: FilePath -> IO ()
solveDay7 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn "Input: "
  mapM_ putStrLn input'lines

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
