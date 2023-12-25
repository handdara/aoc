{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Aoc.Solve.Seventeen
  ( solveDay17,
    -- dbg
    Input,
    Coord (..),
    Direction,
    CityMap,
    DirCount (..),
    unitCoord,
    getAdj,
    solutionPart1,
    solutionPart2,
  )
where

import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Array.IArray ((!))
import qualified Data.Array.IArray as A
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Set as S
import Debug.Trace (trace)
import GHC.Generics (Generic)

-- * Types

type Input = [String]

newtype Coord = Coord (Int, Int) deriving (Show, Eq, Ord, A.Ix, Generic, NFData)

instance Semigroup Coord where
  Coord (xl, yl) <> Coord (xr, yr) = Coord (xl + xr, yl + yr)

instance Monoid Coord where
  mempty = Coord (0, 0)

data Direction = East | North | West | South deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData)

unitCoord :: Direction -> Coord
unitCoord East = Coord (0, 1)
unitCoord North = Coord (-1, 0)
unitCoord West = Coord (0, -1)
unitCoord South = Coord (1, 0)

type HeatLoss = Int

type CityMap = A.Array Coord HeatLoss

data DirCount = Zero | One | Two | Three | Four deriving (Show, Eq, Ord, Enum, Generic, NFData)

data CruciblePos = CP
  { getCoord :: Coord,
    _dir :: Direction,
    _dirCnt :: DirCount
  }
  deriving (Eq, Ord, Generic, NFData)

instance Show CruciblePos where
  show (CP (Coord c) d dc) =
    "{" <> show c <> ", " <> take 1 (show d) <> " for " <> show dc <> "}"

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

allDirs :: [Direction]
allDirs = [minBound .. maxBound]

getAdj :: CityMap -> S.Set CruciblePos -> (CruciblePos, HeatLoss) -> [(CruciblePos, HeatLoss)]
getAdj m visited (CP c d cnt, h) = do
  d'new <- allDirs
  let c'new = c <> unitCoord d'new
  let cnt'new = if d == d'new then succ cnt else One
  let cp'new = CP c'new d'new cnt'new
  ( [ (cp'new, h + heatLossAt c'new)
      | inBounds c'new
          && cnt'new /= Four
          && cp'new `S.notMember` visited
    ]
    )
  where
    inBounds = A.inRange (A.bounds m)
    heatLossAt = (!) m

visitedCoord :: Coord -> S.Set CruciblePos -> Bool
visitedCoord c = S.member c . S.map getCoord

type SearchState = ([(CruciblePos, HeatLoss)], S.Set CruciblePos)

stepSearch :: CityMap -> SearchState -> SearchState
stepSearch m (fs, vs) = (fs'new, vs'new)
  where
    fs'new = (`using` parList rseq) $ do
      f <- fs
      let adj = getAdj m vs f
      if null adj then [f] else adj
    vs'new = S.union vs . S.fromList . map fst $ fs

bfs m = until p (stepSearch m)
  where
    p :: SearchState -> Bool
    p (fs, vs) = all ((`S.member` vs) . fst) fs

stepLowest :: CityMap -> SearchState -> SearchState
stepLowest _ ([], _) = error "stepLowest: empty front list"
stepLowest m (fs@(f : fs'tail), vs) = (fs'new, vs'new)
  where
    f'news = getAdj m vs f
      `using` parList rseq
    fs'new = L.sortBy (compare `on` snd) f'news <> fs'tail
    vs'new = S.union vs . S.fromList . map fst $ fs

tracer msg x = trace (msg <> show x) x

dfs m c = until (visitedCoord c . snd) (stepLowest m)

-- solutionPart1 :: Input -> String
-- solutionPart1 i = coordBfs (Coord (1,3)) $ x0
-- solutionPart1 i = bfs m $ x0
-- solutionPart1 i = step.step $ x0
solutionPart1 i = dfs m (Coord (12,12)) x0
  where
    m = prepareInput i
    cp0 = CP mempty East Zero
    -- cp0 = CP (Coord (0,3)) East Three
    x0 = ([(cp0, 0)], S.empty)
    step = stepLowest m
    coordBfs c = until (visitedCoord c . snd) step

solutionPart2 :: Input -> String
solutionPart2 = const ("In Progress" :: String)

solveDay17 :: FilePath -> IO ()
solveDay17 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn "Input: "
  mapM_ putStrLn input'lines

  let (fs, vs) = solutionPart1 input'lines
  putStrLn "Solution to part 1, fronts: "
  mapM_ print fs
  putStrLn "Solution to part 1, visited: "
  mapM_ print (S.toList vs)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
