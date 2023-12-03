module Aoc.Solve.One
  ( solveDay1,
  )
where

import Data.Char (isDigit, isSpace)
import System.IO
import Data.List (sortBy)

-- * Data Types

type Calories = Integer

type ElfPack = [Calories]

-- | State (pack being built) (prev. built packs)
data State = State ElfPack [ElfPack]

instance Show State where
  show (State b ps) =
    "State: \n  Currently building: "<>show b<>"\n  Built already: "<>show ps

getPacks :: State -> [ElfPack]
getPacks (State _ packs) = packs

emptyState :: State
emptyState = State [] []

-- * Actual Solution

isWhite :: String -> Bool
isWhite = foldr ((&&) . isSpace) True

processLine :: State -> String -> State
processLine (State [] packs) l
  | isWhite l = State [] packs
  | otherwise = State [read (takeWhile isDigit l)] packs
processLine (State b packs) l
  | isWhite l = State [] (b : packs)
  | otherwise = State (read (takeWhile isDigit l) : b) packs

-- | Process all lines and an extra empty line to ensure we fish last build
processInput :: [String] -> State
processInput = flip processLine "" . foldl processLine emptyState

solutionPart1 :: [String] -> Calories
solutionPart1 = maximum . map sum . getPacks . processInput

solutionPart2 :: [String] -> Calories
solutionPart2 = sum . take 3 . sortBy (flip compare) . map sum . getPacks . processInput

solveDay1 :: FilePath -> IO ()
solveDay1 input = do
  input'handle <- openFile input ReadMode
  input'string <- hGetContents input'handle

  let input'lines = lines input'string

  let answerPart1 = solutionPart1 input'lines

  putStrLn $ "Solution to part 1: " ++ show answerPart1

  let answerPart2 = solutionPart2 input'lines

  putStrLn $ "Solution to part 2: " ++ show answerPart2

  hClose input'handle
