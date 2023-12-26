{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Aoc.Solve.Seventeen
  ( solveDay17,
  )
where

import Control.Parallel.Strategies
import Data.Array.IArray ((!))
import qualified Data.Array.IArray as A
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Set as S
import Data.Time.Clock (diffUTCTime, getCurrentTime)

-- * Types

type Input = [String]

newtype Coord = Coord (Int, Int) deriving (Show, Eq, Ord, A.Ix)

instance Semigroup Coord where
  Coord (xl, yl) <> Coord (xr, yr) = Coord (xl + xr, yl + yr)

instance Monoid Coord where
  mempty = Coord (0, 0)

data Direction = East | North | West | South deriving (Show, Eq, Ord, Enum, Bounded)

unitCoord :: Direction -> Coord
unitCoord East = Coord (0, 1)
unitCoord North = Coord (-1, 0)
unitCoord West = Coord (0, -1)
unitCoord South = Coord (1, 0)

type HeatLoss = Int

type CityMap = A.Array Coord HeatLoss

data DirCount = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven
  deriving (Show, Eq, Ord, Enum)

data CruciblePos = CP
  { getCoord :: Coord,
    _dir :: Direction,
    getDirCnt :: DirCount
  }
  deriving (Eq, Ord)

instance Show CruciblePos where
  show (CP (Coord c) d dc) =
    "{" <> show c <> ", " <> take 1 (show d) <> " for " <> show dc <> "}"

type SearchState = ([(CruciblePos, HeatLoss)], S.Set CruciblePos)

-- * Parsing

prepareInput :: Input -> CityMap
prepareInput [] = error "empty input"
prepareInput i@(l : ls)
  | all ((== n) . length) ls = A.listArray (mempty, Coord (m - 1, n - 1)) . map (read . return) . mconcat $ i
  | otherwise = error "uneven input line sizes"
  where
    m = length i
    n = length l

-- * Solutions

changeDir :: Int -> Direction -> Direction
changeDir i d = toEnum $ mod (fromEnum d + i) (1 + fromEnum (maxBound :: Direction))

getAdj :: CityMap -> S.Set CruciblePos -> (CruciblePos, HeatLoss) -> [(CruciblePos, HeatLoss)]
getAdj m visited (CP c d cnt, h) = do
  d'new <- [changeDir (-1) d, d, changeDir 1 d]
  let c'new = c <> unitCoord d'new
  let cnt'new = if d == d'new then succ cnt else One
  let cp'new = CP c'new d'new cnt'new
  [ (cp'new, h + heatLossAt c'new)
    | inBounds c'new
        && cnt'new < Four
        && cp'new `S.notMember` visited
    ]
  where
    inBounds = A.inRange (A.bounds m)
    heatLossAt = (!) m

atCoord :: Coord -> SearchState -> Bool
atCoord c = (c `elem`) . map (getCoord . fst) . fst

stepLowest _ _ ([], _) = error "stepLowest: empty front list"
stepLowest adjFunc m (fs@(f : fs'tail), vs) = (fs'new, vs'new)
  where
    f'news = adjFunc m vs f
    fs'new = L.sortBy (compare `on` snd) $ f'news <> fs'tail
    vs'new = S.union vs . S.fromList . map fst $ fs

dfs :: CityMap -> Coord -> SearchState -> SearchState
dfs m c = until (atCoord c) (stepLowest getAdj m)

solutionPart1 :: Input -> HeatLoss
solutionPart1 i = snd . head . filter ((== tgt) . getCoord . fst) . fst $ dfs m tgt x0
  where
    m = prepareInput i
    tgt = snd . A.bounds $ m
    cp0 = CP mempty East Zero
    x0 = ([(cp0, 0)], S.empty)

getAdjUltra :: CityMap -> S.Set CruciblePos -> (CruciblePos, HeatLoss) -> [(CruciblePos, HeatLoss)]
getAdjUltra m visited (CP c d cnt, h) = do
  d'new <- [changeDir (-1) d, d, changeDir 1 d]
  let c'new = c <> unitCoord d'new
  let goingStraight = d == d'new
  let cnt'new = if goingStraight then succ cnt else One
  let cp'new = CP c'new d'new cnt'new
  [ (cp'new, h + heatLossAt c'new)
    | inBounds c'new
        && cnt'new < Eleven
        && (cnt == Zero || Four <= cnt || goingStraight)
        && cp'new `S.notMember` visited
    ]
  where
    inBounds = A.inRange (A.bounds m)
    heatLossAt = (!) m

atCoordUltra :: Coord -> SearchState -> Bool
atCoordUltra c = any (isEnd . fst) . fst
  where
    isEnd = (&&) <$> (== c) . getCoord <*> ((>= Four) . getDirCnt)

dfsUltra :: CityMap -> Coord -> SearchState -> SearchState
dfsUltra m c = until (atCoordUltra c) (stepLowest getAdjUltra m)

solutionPart2 :: Input -> HeatLoss
solutionPart2 i = snd . head . filter ((== tgt) . getCoord . fst) . fst $ dfsUltra m tgt x0
  where
    m = prepareInput i
    tgt = snd . A.bounds $ m
    cp0 = CP mempty East Zero
    x0 = ([(cp0, 0)], S.empty)

solveDay17 :: FilePath -> IO ()
solveDay17 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  t0 <- getCurrentTime
  let (s1, s2) =
        ((,) <$> solutionPart1 <*> solutionPart2) input'lines
          `using` parTuple2 rseq rseq

  putStr "Solution to part 1: "
  print s1

  putStr "Solution to part 2: "
  print s2
  t1 <- getCurrentTime

  putStrLn $ "Solution time: " <> show (diffUTCTime t1 t0)
