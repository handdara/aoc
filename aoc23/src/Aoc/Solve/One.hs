module Aoc.Solve.One
  ( solveDay1,
  )
where

import qualified Data.List as L
import Debug.Trace (trace)
import System.IO

getFirstAndLast :: String -> (Maybe Char, Maybe Char)
getFirstAndLast = foldl testChar (Nothing, Nothing)
  where
    digits = "0123456789" :: String
    testChar (l'm, r'm) c =
      case l'm of
        Nothing -> if c `elem` digits then (Just c, Just c) else (l'm, r'm)
        Just _ -> if c `elem` digits then (l'm, Just c) else (l'm, r'm)

pairToNum :: (Maybe Char, Maybe Char) -> Maybe Integer
pairToNum (Just l, Just r) = Just (read [l, r])
pairToNum _ = Nothing

solutionPart1 :: [String] -> Maybe Integer
solutionPart1 = fmap sum . mapM (pairToNum . getFirstAndLast)

frontIsDigit :: (Eq a) => [a] -> [a] -> Bool
frontIsDigit s dig = dig == take (length dig) s

leadingDigit :: String -> Maybe Int
leadingDigit = leadingDigit' digits
  where
    digits =
      [ "1",
        "one",
        "2",
        "two",
        "3",
        "three",
        "4",
        "four",
        "5",
        "five",
        "6",
        "six",
        "7",
        "seven",
        "8",
        "eight",
        "9",
        "nine"
      ]
    leadingDigit' [] s = Nothing
    leadingDigit' (d : ds) s =
      if frontIsDigit s d
        then Just (quot (19 - length ds) 2)
        else leadingDigit' ds s

getFirstAndLast'part2 :: String -> (Maybe Int, Maybe Int)
getFirstAndLast'part2 = searchFstLst (Nothing, Nothing)
  where
    searchFstLst x [] = x
    searchFstLst (l'm, r'm) s =
      case leadingDigit s of
        Nothing -> searchFstLst (l'm, r'm) (tail s) 
        Just dig -> case l'm of
          Nothing -> searchFstLst (Just dig, Just dig) (tail s) 
          Just l -> searchFstLst (Just l, Just dig) (tail s) 

pairToNum'part2 :: (Maybe Int, Maybe Int) -> Maybe Int
pairToNum'part2 (Just l, Just r) = Just ( (10 * l) + r )
pairToNum'part2 _ = Nothing

solutionPart2 :: [String] -> Maybe Int
solutionPart2 = fmap sum . mapM (pairToNum'part2 . getFirstAndLast'part2)

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
