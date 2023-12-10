{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Aoc.Solve.Ten
  ( solveDay10,
    -- dbg
    Direction,
    Pipe (..),
    PipeMap,
    PipeDists,
    StepState (..),
    prepareInput,
    connected,
    fdNexts,
    step,
    stepCoords,
    walkFrom,
    mkInitState,
  )
where

import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace (trace)

type Input = [[Char]]

type Coord = (Int, Int)

type Label = Char

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Ord, Enum, Bounded)

data Pipe = Pipe
  { getLabel :: Label,
    getConnections :: S.Set Direction
  }

instance Show Pipe where
  show (Pipe l ds) =
    let n'str = if North `S.member` ds then "N" else "_"
        e'str = if East `S.member` ds then "E" else "_"
        s'str = if South `S.member` ds then "S" else "_"
        w'str = if West `S.member` ds then "W" else "_"
     in "{" <> [l] <> ":" <> n'str <> e'str <> s'str <> w'str <> "}"

type PipeMap = M.Map Coord Pipe

type PipeDists = M.Map Coord Int

data StepState = StepState Int [Coord] PipeDists
  deriving (Show)

-- * Parsing

charToPipe :: Char -> Pipe
charToPipe c =
  case c of
    '|' -> Pipe '|' (S.fromList [North, South])
    '-' -> Pipe '-' (S.fromList [East, West])
    'L' -> Pipe 'L' (S.fromList [North, East])
    'J' -> Pipe 'J' (S.fromList [North, West])
    '7' -> Pipe '7' (S.fromList [West, South])
    'F' -> Pipe 'F' (S.fromList [East, South])
    '.' -> Pipe '.' S.empty
    'S' -> Pipe 'S' (S.fromList [minBound .. maxBound])
    _ -> error "charToPipe: unexpected char"

parseLine :: Int -> Int -> [Char] -> PipeMap -> PipeMap
parseLine _ _ [] pm = pm
parseLine x y (c : cs) pm = parseLine (x + 1) y cs pm'new
  where
    pm'new = M.insert (x, y) (charToPipe c) pm

prepareInput :: Input -> PipeMap
prepareInput ls = foldr (\(l, y) acc -> parseLine 0 y l acc) M.empty ls'is
  where
    ls'is = zip (reverse ls) [0 ..]

-- * Solutions

tracer msg x = trace (msg <> show x) x

connected :: PipeMap -> Coord -> Coord -> Bool
connected pm l@(x'l, y'l) r@(x'r, y'r) = connHori || connVert
  where
    (Pipe _ ds'l) = pm ! l
    (Pipe _ ds'r) = pm ! r
    dx = x'r - x'l
    dy = y'r - y'l
    connHori =
      dy == 0
        && ( (dx == -1 && West `S.member` ds'l && East `S.member` ds'r)
               || (dx == 1 && West `S.member` ds'r && East `S.member` ds'l)
           )
    connVert =
      dx == 0
        && ( (dy == -1 && North `S.member` ds'r && South `S.member` ds'l)
               || (dy == 1 && North `S.member` ds'l && South `S.member` ds'r)
           )

fdNexts :: PipeMap -> Coord -> [Coord]
fdNexts pm c@(x, y) = filter (connected pm c) . foldr addCoord [] $ ds
  where
    (Pipe _ ds) = pm ! c
    addCoord d acc = case d of
      North -> (x, y + 1) : acc
      East -> (x + 1, y) : acc
      South -> (x, y - 1) : acc
      West -> (x - 1, y) : acc

stepCoords :: PipeMap -> [Coord] -> [Coord]
stepCoords pm cs = cs >>= fdNexts pm

step :: PipeMap -> StepState -> StepState
step pm (StepState s cs pds) = StepState s'new cs'new pds'new
  where
    s'new = s + 1
    cs'new = filter (`M.notMember` pds) . stepCoords pm $ cs
    pds'new = foldr (`M.insert` s'new) pds cs'new

mkInitState c = StepState 0 [c] M.empty

walkFrom :: PipeMap -> Coord -> PipeDists
walkFrom pm c = pds
  where
    (StepState _ _ pds) = until (\(StepState _ cs _) -> null cs) (step pm) (StepState 0 [c] M.empty)

fdS :: PipeMap -> Coord
fdS =
  fst
    . head
    . filter (\(_, Pipe l _) -> l == 'S')
    . M.toList

walkFromS :: PipeMap -> PipeDists
walkFromS pm = walkFrom pm s'coord
  where
    s'coord = fdS pm

fdMaxDist :: PipeDists -> Int
fdMaxDist = M.foldr max 0

solutionPart1 :: Input -> Int
solutionPart1 = fdMaxDist . walkFromS . prepareInput

stripCoords :: PipeDists -> S.Set Coord
stripCoords = S.fromList . M.keys

getAdjacentToLoopCoord :: S.Set Coord -> Coord -> S.Set Coord
getAdjacentToLoopCoord cs (x, y) = S.fromList . filter (`S.notMember` cs) $ cs'adj
  where
    cs'adj = [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

solutionPart2 :: Input -> PipeDists
solutionPart2 = walkFromS . prepareInput

solveDay10 :: FilePath -> IO ()
solveDay10 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn "Input: "
  mapM_ putStrLn input'lines

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
