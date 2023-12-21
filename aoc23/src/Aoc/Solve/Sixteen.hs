{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Aoc.Solve.Sixteen
  ( solveDay16,
    -- dbg
    Input,
    solutionPart1,
    solutionPart2,
  )
where

import qualified Data.Array.IArray as A
import Data.Monoid (Sum)

-- * Types

type Input = [String]

data Tile = Empty | VSplit | HSplit | UMirror | DMirror
  deriving (Eq, Ord)

instance Show Tile where
  show = return . tileToChar

type Coord = (Sum Int, Sum Int)

type Direction = Coord

east :: Direction
east = (0, 1)

north :: Direction
north = (1, 0)

west :: Direction
west = (0, -1)

south :: Direction
south = (-1, 0)

data BeamTip = BeamTip Direction Coord

type MirrorMchn = A.Array Coord Tile

-- * Parsing

tileToChar :: Tile -> Char
tileToChar Empty = '.'
tileToChar VSplit = '|'
tileToChar HSplit = '-'
tileToChar UMirror = '/'
tileToChar DMirror = '\\'

charToTile :: Char -> Tile
charToTile '.' = Empty
charToTile '|' = VSplit
charToTile '-' = HSplit
charToTile '/' = UMirror
charToTile '\\' = DMirror
charToTile c = error $ "charToTile: unexpected char: " <> [c]

prepareInput :: Input -> MirrorMchn
prepareInput = undefined

-- * Solution

updateBeamTip :: Tile -> BeamTip -> [BeamTip]
updateBeamTip VSplit (BeamTip d c) | d == east || d == west = [BeamTip north (c <> north), BeamTip south (c<>south)]
updateBeamTip HSplit (BeamTip d c) | d == north || d == south = [BeamTip east (c <> east), BeamTip west (c<>west)]
updateBeamTip UMirror (BeamTip d c) 
  | d == east = [BeamTip north (c <> north)]
  | d == north = [BeamTip east (c <> east)]
  | d == west = [BeamTip south (c <> south)]
  | d == south = [BeamTip west (c <> west)]
updateBeamTip DMirror (BeamTip d c) 
  | d == east = [BeamTip south (c <> south)]
  | d == north = [BeamTip west (c <> west)]
  | d == west = [BeamTip north (c <> north)]
  | d == south = [BeamTip east (c <> east)]
updateBeamTip _ (BeamTip d c) = [BeamTip d (c <> d)]

solutionPart1 :: Input -> String
solutionPart1 = const ("In Progress" :: String)

solutionPart2 :: Input -> String
solutionPart2 = const ("In Progress" :: String)

solveDay16 :: FilePath -> IO ()
solveDay16 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn "Input: "
  mapM_ putStrLn input'lines

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
