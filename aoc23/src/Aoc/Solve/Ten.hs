module Aoc.Solve.Ten
  ( solveDay10,
  )
where

import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- * Types

type Input = [[Char]]

type Coord = (Int, Int)

type Label = Char

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Ord, Enum, Bounded)

data Pipe = Pipe Label (S.Set Direction)

instance Show Pipe where
  show (Pipe l ds) =
    let n'str = if North `S.member` ds then "N" else "_"
        e'str = if East `S.member` ds then "E" else "_"
        s'str = if South `S.member` ds then "S" else "_"
        w'str = if West `S.member` ds then "W" else "_"
     in "{" <> [l] <> ":" <> n'str <> e'str <> s'str <> w'str <> "}"

type PipeMap = M.Map Coord Pipe

type PipeDists = M.Map Coord Int

data StepState
  = StepState Int [Coord] PipeDists
  deriving (Show)

data InState 
  = Outside 
  | IncomingNorthPipe 
  | IncomingSouthPipe 
  | OutgoingNorthPipe 
  | OutgoingSouthPipe 
  | Inside 
  deriving (Show)

data ScanState = ScanState
  { _at :: Coord,
    _prev :: InState,
    _ins :: S.Set Coord
  }
  deriving (Show)

-- * Parsing

allDirs :: S.Set Direction
allDirs = S.fromList [minBound .. maxBound]

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
    'S' -> Pipe 'S' allDirs
    _ -> error "charToPipe: unexpected char"

parseLine :: Int -> Int -> [Char] -> PipeMap -> PipeMap
parseLine _ _ [] pm = pm
parseLine x y (c : cs) pm = parseLine (x + 1) y cs pm'new
  where
    pm'new = M.insert (x, y) (charToPipe c) pm

mkPipeMap :: Input -> PipeMap
mkPipeMap ls = foldr (\(l, y) acc -> parseLine 0 y l acc) M.empty ls'is
  where
    ls'is = zip (reverse ls) [0 ..]

prepareInput :: Input -> PipeMap
prepareInput i =
  let pm = mkPipeMap i
      s'coord = fdS pm
      s'pipe'new = Pipe 'S' (S.filter (dirConnected pm s'coord) allDirs)
   in M.insert s'coord s'pipe'new pm

-- * Solutions

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

mvDir :: Coord -> Direction -> Coord
mvDir (x, y) d = case d of
  North -> (x, y + 1)
  East -> (x + 1, y)
  South -> (x, y - 1)
  West -> (x - 1, y)

dirConnected :: PipeMap -> Coord -> Direction -> Bool
dirConnected pm c d =
  let c'look = mvDir c d
   in connected pm c c'look

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

mkInitScanState :: ScanState
mkInitScanState = ScanState c'init Outside S.empty
  where
    c'init = (0, 0)

stepScan :: PipeMap -> S.Set Coord -> ScanState -> ScanState
stepScan pm cs'loop (ScanState c@(x, y) prevInState ins)
  | c'east `M.notMember` pm = ScanState (0, y + 1) Outside ins
  | otherwise = case prevInState of
      Outside ->
        let onLoopPipe = c `S.member` cs'loop
            southConnected = South `S.member` ds
            northConnected = North `S.member` ds
            currInState =
              case (onLoopPipe, southConnected, northConnected) of
                (False, _, _) -> Outside
                (_, True,  True) -> Inside
                (_, True, False) -> IncomingSouthPipe
                (_, False, True) -> IncomingNorthPipe
                _ -> error "stepScan: either north or south must be connected on pipe hit fr outside"
         in ScanState c'east currInState ins
      IncomingNorthPipe ->
        let southConnected = South `S.member` ds
            northConnected = North `S.member` ds
            currInState = case (southConnected, northConnected) of
              (False,  True) -> Outside
              ( True, False) -> Inside
              (False, False) -> IncomingNorthPipe 
              _ -> error "stepScan: IncomingNorthPipe: hit `|`"
         in ScanState c'east currInState ins
      IncomingSouthPipe ->
        let southConnected = South `S.member` ds
            northConnected = North `S.member` ds
            currInState = case (southConnected, northConnected) of
              (False,  True) -> Inside
              ( True, False) -> Outside
              (False, False) -> IncomingSouthPipe 
              _ -> error "stepScan: IncomingSouthPipe: hit `|`"
         in ScanState c'east currInState ins
      OutgoingNorthPipe ->
        let southConnected = South `S.member` ds
            northConnected = North `S.member` ds
            currInState = case (southConnected, northConnected) of
              (False,  True) -> Inside
              ( True, False) -> Outside
              (False, False) -> OutgoingNorthPipe 
              _ -> error "stepScan: InNorthPipe: hit `|`"
         in ScanState c'east currInState ins
      OutgoingSouthPipe ->
        let southConnected = South `S.member` ds
            northConnected = North `S.member` ds
            currInState = case (southConnected, northConnected) of
              (False,  True) -> Outside
              ( True, False) -> Inside
              (False, False) -> OutgoingSouthPipe
              _ -> error "stepScan: InNorthPipe: hit `|`"
         in ScanState c'east currInState ins
      Inside ->
        let onLoopPipe = c `S.member` cs'loop
            southConnected = South `S.member` ds
            northConnected = North `S.member` ds
            currInState =
              case (onLoopPipe, southConnected, northConnected) of
                (False, _, _) -> Inside
                (_, True,  True) -> Outside
                (_, True, False) -> OutgoingSouthPipe
                (_, False, True) -> OutgoingNorthPipe
                _ -> error "stepScan: Inside: went straight to `-`"
            ins'next = case currInState of
              Inside -> S.insert c ins
              _ -> ins
         in ScanState c'east currInState ins'next
  where
    c'east = (x + 1, y)
    (Pipe _ ds) = pm ! c

scan :: PipeMap -> S.Set Coord -> ScanState -> ScanState
scan pm cs'loop = until outOfUpperBound (stepScan pm cs'loop)
  where
    (_, y'max) = maximum . M.keys $ pm
    outOfUpperBound (ScanState (_, y) _ _) = y'max < y

solutionPart2 :: Input -> Int
solutionPart2 i = length ins
  where
    pm = prepareInput i
    cs'loop = stripCoords . walkFromS $ pm
    state'0 = mkInitScanState
    state'f = scan pm cs'loop state'0
    (ScanState _ _ ins) = state'f

solveDay10 :: FilePath -> IO ()
solveDay10 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
