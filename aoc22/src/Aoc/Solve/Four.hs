{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Aoc.Solve.Four
  ( solveDay4,
  )
where

import qualified Data.List as L

type Input = [String]

type Range = (Int, Int)

data Pair = Pair Range Range deriving (Show)

rangeInRange (lMin, lMax) (rMin, rMax) = lMin <= rMin && rMax <= lMax

testPair (Pair l r) = rangeInRange l r || rangeInRange r l

testPairOverlap (Pair (lMin, lMax) (rMin, rMax)) = not $ lMax < rMin || rMax < lMin

parsePair :: String -> Pair
parsePair s = Pair (read lMin'str, read . tail $ lMax'str) (read rMin'str, read . tail $ rMax'str)
  where
    (lr,rr) = span (/=',') s
    (lMin'str,lMax'str) = span (/='-') lr
    (rMin'str,rMax'str) = span (/='-') . tail $ rr

solutionPart1 = length . filter testPair . map parsePair

solutionPart2 = length . filter testPairOverlap . map parsePair

solveDay4 :: FilePath -> IO ()
solveDay4 input = do
  input'string <- readFile input

  let puzzleInput = lines input'string :: Input

  let answerPart1 = solutionPart1 puzzleInput

  putStrLn $ "Solution to part 1: " ++ show answerPart1

  let answerPart2 = solutionPart2 puzzleInput

  putStrLn $ "Solution to part 2: " ++ show answerPart2
