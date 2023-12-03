module Aoc.Solve.Two
  ( solveDay2,
  )
where

import System.IO

type Input = [String]

data Move = Rock | Scissors | Paper deriving (Eq, Ord, Bounded, Show)

data Round = Round Move Move

myCodeToMove :: Char -> Move
myCodeToMove 'X' = Rock
myCodeToMove 'Y' = Paper
myCodeToMove 'Z' = Scissors
myCodeToMove c = error $ "Unexpected character: " <> [c]

oppCodeToMove :: Char -> Move
oppCodeToMove 'A' = Rock
oppCodeToMove 'B' = Paper
oppCodeToMove 'C' = Scissors
oppCodeToMove c = error $ "Unexpected character: " <> [c]

parseRound :: String -> Round
parseRound [oppCode, _, myCode] = Round (oppCodeToMove oppCode) (myCodeToMove myCode)
parseRound l = error $ "Unexpected input format, input: " <> l

valueOf :: (Num a) => Move -> a
valueOf Rock = 1
valueOf Paper = 2
valueOf Scissors = 3

winValue :: Int
winValue = 6

drawValue :: Int
drawValue = 3

lossValue :: Int
lossValue = 0

outcomeValue :: Move -> Move -> Int
outcomeValue Rock Paper = winValue
outcomeValue Scissors Rock = winValue
outcomeValue Paper Scissors = winValue
outcomeValue Rock Rock = drawValue
outcomeValue Paper Paper = drawValue
outcomeValue Scissors Scissors = drawValue
outcomeValue _ _ = lossValue

scoreRound :: Round -> Int
scoreRound (Round oppMv myMv) = outcomeValue oppMv myMv + valueOf myMv

solutionPart1 :: [String] -> Int
solutionPart1 = sum . map (scoreRound . parseRound)

myCodeToMove'part2 :: Move -> Char -> Move
myCodeToMove'part2 oppMove 'Y' = oppMove
myCodeToMove'part2 Rock 'X' = Scissors
myCodeToMove'part2 Paper 'X' = Rock
myCodeToMove'part2 Scissors 'X' = Paper
myCodeToMove'part2 Scissors 'Z' = Rock
myCodeToMove'part2 Rock 'Z' = Paper
myCodeToMove'part2 Paper 'Z' = Scissors
myCodeToMove'part2 _ c = error $ "Unexpected character: " <> [c]

parseRound'part2 :: String -> Round
parseRound'part2 [oppCode, _, myCode] =
  let oppMove = oppCodeToMove oppCode
   in Round oppMove (myCodeToMove'part2 oppMove myCode)
parseRound'part2 l = error $ "Unexpected input format, input: " <> l

solutionPart2 :: [String] -> Int
solutionPart2 = sum . map (scoreRound . parseRound'part2)

solveDay2 :: FilePath -> IO ()
solveDay2 input = do
  input'handle <- openFile input ReadMode
  input'string <- hGetContents input'handle

  let puzzleInput = lines input'string :: Input

  let answerPart1 = solutionPart1 puzzleInput

  putStrLn $ "Solution to part 1: " ++ show answerPart1

  let answerPart2 = solutionPart2 puzzleInput

  putStrLn $ "Solution to part 2: " ++ show answerPart2

  hClose input'handle
