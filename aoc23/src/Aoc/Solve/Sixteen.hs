{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Aoc.Solve.Sixteen
  ( solveDay16,
    -- dbg
    Input,
    solutionPart1,
    solutionPart2,
  )
where

import Data.Monoid (Sum)
import qualified Data.Array.IArray as A

-- * Types

type Input = [String]

data Direction = East | North | West | South 
  deriving (Eq, Ord)

data Tile = Empty | VSplit | HSplit | UMirror | DMirror 
  deriving (Eq, Ord)

instance Show Tile where
  show = return . tileToChar

type Coord = Sum (Int,Int)

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
