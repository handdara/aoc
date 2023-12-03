module Aoc.Solve.Three
  ( solveDay3,
  )
where

import Data.Char (isDigit)
import System.IO

-- * Data Types

type NumberVal = Maybe String

data Number = Number Int Int Int Int

getValue :: Number -> Int
getValue (Number _ _ _ v) = v

instance Show Number where
  show (Number r sc ec v) = "{" <> show v <> ": (" <> show r <> "," <> show sc <> ") to (" <> show r <> "," <> show ec <> ")}"

data Symbol = Symbol Int Int

instance Show Symbol where
  show (Symbol r c) = "(" <> show r <> "," <> show c <> ")"

data Gear = Gear Int Int

instance Show Gear where
  show (Gear r c) = "(" <> show r <> "," <> show c <> ")"

data State = State NumberVal [Number] [Symbol] [Gear]

instance Show State where
  show (State b ns ss gs) = "State \n  Building Num: " <> show b <> "\n  Nums: " <> show ns <> "\n  Syms: " <> show ss <> "\n  Gears: " <> show gs

emptyState :: State
emptyState = State Nothing [] [] []

-- * Actual Solution

mkNum :: Int -> Int -> String -> Number
mkNum row colTerm s = Number row (colTerm - length s) (colTerm - 1) (read s)

convertPartialRow :: State -> Int -> Int -> String -> State
convertPartialRow (State b ns ss gs) row col [] =
  case b of
    Nothing -> State Nothing ns ss gs
    Just ds -> State Nothing (mkNum row col ds : ns) ss gs
convertPartialRow (State b ns ss gs) row col (c : cs)
  | isDigit c = case b of
      Nothing -> convertPartialRow (State (Just [c]) ns ss gs) row (col + 1) cs
      Just ds -> convertPartialRow (State (Just (ds <> [c])) ns ss gs) row (col + 1) cs
  | c == '.' = case b of
      Nothing -> convertPartialRow (State b ns ss gs) row (col + 1) cs
      Just ds -> convertPartialRow (State Nothing (mkNum row col ds : ns) ss gs) row (col + 1) cs
  | c == '*' = case b of
      Nothing ->
        convertPartialRow
          (State Nothing ns (Symbol row col : ss) (Gear row col : gs))
          row
          (col + 1)
          cs
      Just ds ->
        convertPartialRow
          (State Nothing (mkNum row col ds : ns) (Symbol row col : ss) (Gear row col : gs))
          row
          (col + 1)
          cs
  | otherwise = case b of
      Nothing -> convertPartialRow (State Nothing ns (Symbol row col : ss) gs) row (col + 1) cs
      Just ds -> convertPartialRow (State Nothing (mkNum row col ds : ns) (Symbol row col : ss) gs) row (col + 1) cs

convertRow :: State -> Int -> String -> State
convertRow state row = convertPartialRow state row 0

convertRows :: State -> Int -> [String] -> State
convertRows state _ [] = state
convertRows state row (l : ls) = convertRows state'processed (row + 1) ls
  where
    state'processed = convertRow state row l

processInput :: [String] -> State
processInput = convertRows emptyState 0

isAdjacent :: Int -> Int -> Number -> Bool
isAdjacent tr tc (Number nr nsc nec _) = minRow <= tr && tr <= maxRow && minCol <= tc && tc <= maxCol
  where
    minRow = nr - 1
    maxRow = nr + 1
    minCol = nsc - 1
    maxCol = nec + 1

testNumber :: [Symbol] -> Number -> Bool
testNumber other'syms num =
  foldr (\(Symbol r c) -> (||) (isAdjacent r c num)) False other'syms

getValidNumbers :: State -> [Number]
getValidNumbers (State _ ns ss _) = filter (testNumber ss) ns

solutionPart1 :: [String] -> Int
solutionPart1 = sum . map getValue . getValidNumbers . processInput

getAdjacent :: [Number] -> Gear -> [Number]
getAdjacent ns (Gear r c) = filter (isAdjacent r c) ns

getPairList :: State -> [[Number]]
getPairList (State _ ns _ gs) = keepPairs . getLists $ gs
  where
    keepPairs = filter ((==) 2 . length)
    getLists = map (getAdjacent ns)

prodPair :: [Number] -> Int
prodPair (f : s : _) = getValue f * getValue s
prodPair _ = error "less than two elements"

solutionPart2 :: [String] -> Int
solutionPart2 = sum . map prodPair . getPairList . processInput

solveDay3 :: FilePath -> IO ()
solveDay3 input'path = do
  input'handle <- openFile input'path ReadMode
  input'string <- hGetContents input'handle

  let input'lines = lines input'string

  putStrLn $ "Solution to part 1: " ++ show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " ++ show (solutionPart2 input'lines)

  hClose input'handle
