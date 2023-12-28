{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Aoc.Solve.Eighteen
  ( solveDay18,
    -- dbg
    Input,
    Tile (..),
    solutionPart1,
    solutionPart2,
  )
where

import Aoc.Parse
import Control.Applicative
import Data.Bits (xor)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import GHC.Generics (Generic1, Generically1 (..))

-- * Types

type Input = [String]

data Direction = East | North | West | South deriving (Show, Eq, Ord, Enum, Bounded)

type Meter = Int

type Color = String

data Instruction = Inst
  { _dir :: Direction,
    _len :: Meter,
    _col :: Color
  }
  deriving (Eq, Ord)

instance Show Instruction where
  show (Inst d l c) = "{" <> take 1 (show d) <> "," <> show l <> "," <> show c <> "}"

type Coord = (Meter, Meter)

data Tile a = Tile {_E, _N, _W, _S :: Maybe a}
  deriving (Show, Eq, Ord, Functor, Generic1)
  deriving (Applicative, Alternative) via Generically1 Tile

ewTile x = Tile (Just x) Nothing (Just x) Nothing

nsTile x = Tile Nothing (Just x) Nothing (Just x)

type Hole = Tile Color

type DigPlan = M.Map Coord Hole

data ScanState = ScSt {getSum :: Int, _above, _below, _inside :: Bool} deriving (Show)

-- * Parsing

dirP :: Parser Direction
dirP = eastP <|> northP <|> westP <|> southP
  where
    eastP = mkCharParser 'R' $> East
    northP = mkCharParser 'U' $> North
    westP = mkCharParser 'L' $> West
    southP = mkCharParser 'D' $> South

meterP :: Parser Meter
meterP = intParser

colorP :: Parser Color
colorP = mkStringParser "(#" *> mkTakeParser 6 <* mkCharParser ')'

instP :: Parser Instruction
instP =
  Inst
    <$> (dirP <* whiteParser)
    <*> (meterP <* whiteParser)
    <*> colorP

prepareInput = map (fromMaybe (error "parse failed") . tryParse instP)

-- * Solution

processInst :: (Coord, DigPlan) -> Instruction -> (Coord, DigPlan)
processInst ((i, j), dpM) (Inst d l col) =
  (,)
    (last cs)
    (foldl' (\acc c -> M.insertWith (<|>) c hole acc) dpM cs)
  where
    idxs = [1 .. l]
    cs = case d of
      North -> (,j) . (i -) <$> idxs
      South -> (,j) . (i +) <$> idxs
      West -> (i,) . (j -) <$> idxs
      East -> (i,) . (j +) <$> idxs
    hole =
      if d == North || d == South
        then ewTile col
        else nsTile col

mkDigPlan :: [Instruction] -> DigPlan
mkDigPlan = snd . foldl' processInst ((0, 0), M.singleton (0, 0) empty)

fdArea :: DigPlan -> Int
fdArea dp = sum . map (getSum . scanRow) $ [i'min .. i'max]
  where
    ks = M.keys dp
    (i'min, i'max) = (,) <$> minimum <*> maximum $ map fst ks
    (j'min, j'max) = (,) <$> minimum <*> maximum $ map snd ks
    scanRow i = foldl' processCell initScanSt [j'min .. j'max]
      where
        initScanSt = ScSt 0 False False False
        processCell :: ScanState -> Int -> ScanState
        processCell (ScSt acc above below inside) j =
          let onHole = (i, j) `M.member` dp
              holeIsAbove = (i - 1, j) `M.member` dp
              holeIsBelow = (i + 1, j) `M.member` dp
              above' = ((onHole && holeIsAbove) || above)
              below' = ((onHole && holeIsBelow) || below)
              inside' = (onHole && above' && below') `xor` inside
              (above'', below'') =
                if inside /= inside' || not onHole
                  then (False, False)
                  else (above', below')
              acc' = if onHole || inside then acc + 1 else acc
           in ScSt acc' above'' below'' inside'

solutionPart1 :: Input -> Int
solutionPart1 = fdArea . mkDigPlan . prepareInput

solutionPart2 :: Input -> String
solutionPart2 = const ("In Progress" :: String)

solveDay18 :: FilePath -> IO ()
solveDay18 input'path = do
  input'string <- readFile input'path

  let input'lines = takeWhile (not . lineEmpty) . lines $ input'string

  -- putStrLn "Input: "
  -- mapM_ putStrLn input'lines

  let s1 = solutionPart1 input'lines
  putStrLn $ "Solution to part 1: " <> show s1

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
