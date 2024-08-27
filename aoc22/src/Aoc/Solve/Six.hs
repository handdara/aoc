module Aoc.Solve.Six
  ( solveDay6,
  )
where

type Input = String

allDiff :: Eq a => [a] -> Bool
allDiff [] = True
allDiff (x : xs) = x `notElem` xs && allDiff xs

search :: Int -> Int -> String -> Maybe Int
search numUnique idx cs
  | length cs < numUnique = Nothing
  | otherwise = if allDiff (take numUnique cs) 
    then Just (idx + numUnique)
    else search numUnique (idx + 1) (tail cs)

solutionPart1 :: Input -> Maybe Int
solutionPart1 = search 4 0

solutionPart2 :: Input -> Maybe Int
solutionPart2 = search 14 0

solveDay6 :: FilePath -> IO ()
solveDay6 input = do
  input'string <- readFile input

  let puzzleInput = input'string

  let answerPart1 = solutionPart1 puzzleInput

  putStrLn $ "Solution to part 1: " <> show answerPart1

  let answerPart2 = solutionPart2 puzzleInput

  putStrLn $ "Solution to part 2: " <> show answerPart2
