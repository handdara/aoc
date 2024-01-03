{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Aoc.Solve.Twenty
  ( solveDay20,
    -- dbg
    Input,
    solutionPart1,
    solutionPart2,
  )
where

import Aoc.Parse
import qualified Data.Map.Strict as M

-- * Types

type Input = [String]

type ModuleLabel = String

type RxMemory = M.Map ModuleLabel Bool

data ModuleState = FlipFlop Bool | Conjuction RxMemory | Broadcast

-- * Parsing

-- * Solutions

solutionPart1 :: Input -> String
solutionPart1 = const ("In Progress" :: String)

solutionPart2 :: Input -> String
solutionPart2 = const ("In Progress" :: String)

solveDay20 :: FilePath -> IO ()
solveDay20 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn "Input: "
  mapM_ putStrLn input'lines

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
