{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Aoc.Solve.Three
  ( solveDay3,
  )
where

import Data.Char (isDigit)
import System.IO

-- * Data Types

type NumberVal = Maybe String

data Number = Number
  { getRow :: Int,
    getStartCol :: Int,
    getEndCol :: Int,
    getValue :: Int
  }
  deriving (Show)

data Symbol = Symbol
  { getSymRow :: Int,
    getSymCol :: Int
  }
  deriving (Show)

data State = State
  { currNumBuild :: NumberVal,
    currNums :: [Number],
    currSyms :: [Symbol]
  }
  deriving (Show)

-- * Actual Solution

mkNum :: Int -> Int -> String -> Number
mkNum row colTerm s = Number row (colTerm - length s) (colTerm - 1) (read s)

mkSym row colTerm = Symbol row (colTerm - 1)

convertRow :: State -> Int -> Int -> String -> State
convertRow state _ _ _ = state
convertRow state row col (c : cs)
  | isDigit c = case currNumBuild state of
      Nothing -> convertRow (State (Just [c]) (currNums state) (currSyms state)) row (col + 1) cs
      Just ds -> convertRow (State (Just (ds<>[c])) (currNums state) (currSyms state)) row (col + 1) cs 
  | c == '.' = case currNumBuild state of 
      Nothing -> convertRow state row (col + 1) cs
      Just ds -> convertRow (State Nothing (mkNum row col ds : currNums state) (currSyms state)) row (col + 1) cs 
  | otherwise = case currNumBuild state of 
    Nothing -> convertRow (State Nothing (currNums state) (mkSym row col : currSyms state)) row (col + 1) cs
    Just ds -> undefined

solutionPart1 _ = "In Progress"

solutionPart2 _ = "In Progress"

solveDay3 input'path = do
  input'handle <- openFile input'path ReadMode
  input'string <- hGetContents input'handle

  let input'lines = lines input'string

  -- putStrLn $ "Valid games: " ++ show games'valid'ids
  putStrLn $ "Solution to part 1: " ++ show (solutionPart1 input'lines)

  -- putStrLn $ "Powers: " ++ show games'powers
  putStrLn $ "Solution to part 2: " ++ show (solutionPart2 input'lines)

  hClose input'handle
