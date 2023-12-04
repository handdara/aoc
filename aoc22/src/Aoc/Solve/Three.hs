module Aoc.Solve.Three
  ( solveDay3,
  )
where

import Control.Monad ((<=<))
import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Set as S

type Input = [[Item]]

type Item = Char

type Priority = Int

data Rucksack = Rucksack [Item] [Item]

getPriority :: Item -> Maybe Priority
getPriority = return . (+ 1) <=< flip L.elemIndex order
  where
    order = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" :: [Item]

parseRucksack :: [Item] -> Rucksack
parseRucksack is = Rucksack left right
  where
    l = length is `quot` 2
    (left, right) = splitAt l is

findDuplicates :: Rucksack -> S.Set Item
findDuplicates (Rucksack left right) = S.fromList $ left `L.intersect` right

findDuplicate :: Rucksack -> Item
findDuplicate r
  | S.size ds == 1 = S.findMin ds
  | otherwise = error "number of duplicates /= 1"
  where
    ds = findDuplicates r

calcPriority :: Rucksack -> Priority
calcPriority = fromJust . getPriority . findDuplicate

solutionPart1 :: Input -> Int
solutionPart1 = sum . map (calcPriority . parseRucksack)

parseItemSet :: [Item] -> S.Set Item
parseItemSet = foldr S.insert S.empty

processGroups :: [Item] -> [S.Set Item] -> [Item]
processGroups is [] = is
processGroups is (x : y : z : rest) = processGroups is'new rest
  where
    inAllThree = S.intersection x (S.intersection y z)
    is'new = S.findMin inAllThree : is
processGroups _ _ = error "3 doesn't divide number of rucksacks"

solutionPart2 :: Input -> Maybe Priority
solutionPart2 = pure . sum <=< mapM getPriority . processGroups [] . map parseItemSet

solveDay3 :: FilePath -> IO ()
solveDay3 input = do
  input'string <- readFile input

  let puzzleInput = lines input'string :: Input

  let answerPart1 = solutionPart1 puzzleInput

  putStrLn $ "Solution to part 1: " ++ show answerPart1

  let answerPart2 = solutionPart2 puzzleInput

  putStrLn $ "Solution to part 2: " ++ show answerPart2
