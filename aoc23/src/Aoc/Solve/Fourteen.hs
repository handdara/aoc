{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Aoc.Solve.Fourteen
  ( solveDay14,
  )
where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)

-- * Types

type Input = [String]

data Rock = None | Cube | Round
  deriving (Show, Eq, Ord, Enum)

type Platform = [[Rock]]

type Scan = [(Int, Int)]

-- * Parsing

charToRock :: Char -> Rock
charToRock 'O' = Round
charToRock '#' = Cube
charToRock '.' = None
charToRock c = error $ "unexpected char in parse: " <> [c]

prepareInput :: Input -> Platform
prepareInput = map (map charToRock)

-- * Solutions

stepScan :: Scan -> Rock -> Scan
stepScan [] Cube = replicate 2 (0, 0)
stepScan [] Round = [(1, 0)]
stepScan [] None = [(0, 1)]
stepScan s Cube = (0, 0) : s
stepScan ((rounds, nones) : rest) Round = (rounds + 1, nones) : rest
stepScan ((rounds, nones) : rest) None = (rounds, nones + 1) : rest

scanRow :: [Rock] -> Scan
scanRow = foldl stepScan []

reconstituteRow :: Scan -> [Rock]
reconstituteRow = L.intercalate [Cube] . map mkRocks . reverse
  where
    mkRocks (rs, ns) = replicate rs Round <> replicate ns None

mvRow :: [Rock] -> [Rock]
mvRow = reconstituteRow . scanRow

tiltWest :: Platform -> Platform
tiltWest = map mvRow

tiltNorth :: Platform -> Platform
tiltNorth = L.transpose . tiltWest . L.transpose

tiltSouth :: Platform -> Platform
tiltSouth = reverse . tiltNorth . reverse

tiltEast :: Platform -> Platform
tiltEast = map reverse . tiltWest . map reverse

spinCycle :: Platform -> Platform
spinCycle = tiltEast . tiltSouth . tiltWest . tiltNorth

northBeamLoad :: Platform -> Int
northBeamLoad p =
  let rowCounts = map (length . filter (== Round)) p
      rowWeights = zipWith (*) (reverse rowCounts) [1 ..]
   in sum rowWeights

solutionPart1 :: Input -> Int
solutionPart1 = northBeamLoad . tiltNorth . prepareInput

fdCycleLoop :: Int -> Platform -> (Platform, M.Map Platform [Int], Int)
fdCycleLoop maxCycles p =
  let x0 = (p, M.empty, 0) :: (Platform, M.Map Platform [Int], Int)
      step (p's, m's, s) = (spinCycle p's, M.insertWith (<>) p's [s] m's, s + 1)
      xfm1 = until (\(p', m', s') -> s' == maxCycles - 1 || isJust (m' M.!? p')) step x0
   in step xfm1

spinCycleL :: Int -> Platform -> Platform
spinCycleL cycles p =
  let (p'srch, m'srch, s'srch) = fdCycleLoop cycles p
   in if s'srch == cycles
        then p'srch
        else
          let [(p'loopStart, [s2, s1])] = filter (\(_, ns) -> length ns >= 2) . M.toList $ m'srch
              dl = (cycles - s1) `mod` (s2 - s1)
           in spinCycleL dl p'loopStart

solutionPart2 :: Int -> [String] -> Int
solutionPart2 cycles = northBeamLoad . spinCycleL cycles . prepareInput

solveDay14 :: String -> FilePath -> IO ()
solveDay14 cycles input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)
  putStrLn $ "Solution to part 2: " <> show (solutionPart2 (read cycles) input'lines)
