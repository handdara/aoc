{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Aoc.Solve.Five
  ( solveDay5,
  )
where

import Data.Char (isSpace)
import qualified Data.List as L

type Input = [String]

type Category = String

data Rule = Rule Int Int Int

instance Show Rule where
  show (Rule d s l) = "{Source: " <> show (s, s + l - 1) <> ", Dest: " <> show (d, d + l - 1) <> "}"

data Map = Map Category Category [Rule]

instance Show Map where
  show (Map frCat toCat rules) =
    "\nMap "
      <> frCat
      <> "-to-"
      <> toCat
      <> ":\n  "
      <> L.intercalate "\n  " (map show rules)

type Range = (Int, Int)

data Almanac = Almanac [Range] [Map] deriving (Show)

-- * Parsing

lineEmpty :: String -> Bool
lineEmpty = all isSpace

splitOnEmptyLines :: [String] -> [[String]]
splitOnEmptyLines = foldr foldFunc [[]]
  where
    foldFunc line [] = foldFunc line [[]]
    foldFunc line (g : gs) =
      if lineEmpty line
        then [] : g : gs
        else (line : g) : gs

parseRule :: String -> Rule
parseRule = unwrap . map read . words
  where
    unwrap [d, s, l] = Rule d s l
    unwrap _ = error "parseRule: number count /= 3"

groupLinesToMap :: [String] -> Map
groupLinesToMap [] = error "groupLinesToMap: improper input, given no lines"
groupLinesToMap (title'line : rules'line) = Map fromCat toCat rules
  where
    (fromCat, rest'title) = break (== '-') title'line
    toCat = takeWhile (/= ' ') . tail . dropWhile (/= '-') . tail $ rest'title
    rules = map parseRule rules'line

prepareSeeds'part1 :: [Int] -> [Range]
prepareSeeds'part1 = map (\seed -> (seed, seed))

intPairsToRanges :: [Int] -> [Range] -> [Range]
intPairsToRanges [] = id
intPairsToRanges (start : l : rest) = (newRange :) . intPairsToRanges rest
  where
    newRange = (start, start + l - 1)
intPairsToRanges _ = error "intPairsToRanges: non-even input list length"

prepareSeeds'part2 :: [Int] -> [Range]
prepareSeeds'part2 = flip intPairsToRanges []

-- | Prepares using the assumption that each element in the seed text list is
-- an individual seed to consider
prepareInput'part1 :: Input -> Almanac
prepareInput'part1 i = Almanac seeds maps
  where
    ([seedLine], _ : mapLines) = splitAt 1 i
    (_ : seeds'string) = words seedLine
    seeds'input = map read seeds'string :: [Int]
    seeds = prepareSeeds'part1 seeds'input
    maps = map groupLinesToMap . splitOnEmptyLines $ mapLines

-- | Prepares using the assumption that each pair of elements in the seed text list is
-- a range of seeds to consider
prepareInput'part2 :: Input -> Almanac
prepareInput'part2 i = Almanac seeds maps
  where
    ([seedLine], _ : mapLines) = splitAt 1 i
    (_ : seeds'string) = words seedLine
    seeds'input = map read seeds'string :: [Int]
    seeds = prepareSeeds'part2 seeds'input
    maps = map groupLinesToMap . splitOnEmptyLines $ mapLines

-- * Actual Solution

rangeElem :: Int -> Range -> Bool
rangeElem n (low, high) = low <= n && n <= high

-- | split a range into a range of elements less than a number,
--  and a range of elements greater than or equal to that number
splitBelow :: Int -> Range -> (Maybe Range, Maybe Range)
splitBelow n r@(low, high)
  | n <= low = (Nothing, Just r)
  | high < n = (Just r, Nothing)
  | otherwise = (Just (low, n - 1), Just (n, high))

-- | split a range into a range of elements less than or equal to a number,
--  and a range of elements greater than that number
splitAbove :: Int -> Range -> (Maybe Range, Maybe Range)
splitAbove n r@(low, high)
  | high <= n = (Just r, Nothing)
  | n < low = (Nothing, Just r)
  | otherwise = (Just (low, n), Just (n + 1, high))

splitOnRange ::
  -- | r'map is the range in the map, the range being split "on"
  Range ->
  -- | r'item is the range of items (e.g. a seed range), the range being split
  Range ->
  [Range]
splitOnRange (low'm, high'm) r'item =
  case splitBelow low'm r'item of
    (Just _, Nothing) -> [r'item] -- ranges are disjoint, r'item below r'map
    (Nothing, _) ->
      case splitAbove high'm r'item of
        (Nothing, Just _) -> [r'item] -- ranges are disjoint, r'map below r'item
        (Just _, Nothing) -> [r'item] -- r'item is a subrange of r'map
        (Just r'highIn, Just r'highOut) -> [r'highIn, r'highOut] -- r'map low overlaps r'item
        (Nothing, Nothing) -> error "splitOnRange: splitAbove returned (Nothing, Nothing), this should never happen"
    (Just r'lowOut, Just r'lowIn) ->
      case splitAbove high'm r'lowIn of
        (Just _, Nothing) -> [r'lowOut, r'lowIn] -- r'map high overlaps r'item
        (Just r'in, Just r'highOut) -> [r'lowOut, r'in, r'highOut] -- r'map is a subrange of r'item
        (Nothing, Just _) -> error "splitOnRange: splitAbove returned (Nothing, Just _) with splitBelow result"
        (Nothing, Nothing) -> error "splitOnRange: splitAbove returned (Nothing, Nothing), this should never happen"

-- | Once this function splits a set of ranges, then the following is true
--  Statement. a rule applies to an element of a range <=> the rule applies to all elements of the range
splitRangesOnRules :: [Rule] -> [Range] -> [Range]
splitRangesOnRules [] ranges = ranges
splitRangesOnRules (rule : others) ranges = splitRangesOnRules others ranges'new
  where
    splitWithRule (Rule _ fr l) = splitOnRange (fr, fr + l - 1)
    ranges'new = ranges >>= splitWithRule rule

-- | should only be applied after ranges have been split
applyRule :: Rule -> Range -> Maybe Range
applyRule (Rule to fr l) (low, high) =
  if low `rangeElem` frRange
    then Just (low + delta, high + delta)
    else Nothing
  where
    frRange = (fr, fr + l - 1)
    delta = to - fr

fdAndApplyRule :: [Rule] -> Range -> Range
fdAndApplyRule [] r = r
fdAndApplyRule (rule : others) r =
  case applyRule rule r of
    Nothing -> fdAndApplyRule others r
    Just r' -> r'

mapRange :: Map -> Range -> [Range]
mapRange (Map _ _ rules) range = map (fdAndApplyRule rules) ranges
  where
    ranges = splitRangesOnRules rules [range]

applySingle :: Almanac -> Almanac
applySingle (Almanac ranges []) = Almanac ranges []
applySingle (Almanac ranges (m : ms)) = Almanac ranges'new ms
  where
    ranges'new = ranges >>= mapRange m

applyAll :: Almanac -> Almanac
applyAll (Almanac ranges []) = Almanac ranges []
applyAll a = applyAll . applySingle $ a

getFinalRanges :: Almanac -> [Range]
getFinalRanges a = ranges'end
  where
    (Almanac ranges'end _) = applyAll a

fdMinElem :: [Range] -> Int
fdMinElem = minimum . map fst

solutionPart1 :: Input -> Int
solutionPart1 = fdMinElem . getFinalRanges . prepareInput'part1

solutionPart2 :: Input -> Int
solutionPart2 = fdMinElem . getFinalRanges . prepareInput'part2

solveDay5 :: FilePath -> IO ()
solveDay5 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
