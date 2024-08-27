{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Aoc.Solve.Eighteen
  ( solveDay18,
  )
where

import Aoc.Parse
import Control.Applicative
import Control.DeepSeq (NFData, deepseq)
import Data.Bits (xor)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import GHC.Generics (Generic, Generic1, Generically1 (..))

-- * Types

type Input = [String]

data Direction = East | North | West | South deriving (Show, Eq, Ord, Enum, Bounded)

type Meter = Int

type Coord = (Meter, Meter)

type Color = String

data Instruction = Inst
  { _dir :: Direction,
    _len :: Meter,
    getColor :: Color
  }
  deriving (Eq, Ord)

instance Show Instruction where
  show (Inst d l c) = "{" <> take 1 (show d) <> "," <> show l <> "," <> show c <> "}"

data Tile a = Tile {_E, _N, _W, _S :: Maybe a}
  deriving (Show, Eq, Ord, Functor, Generic1)
  deriving (Applicative, Alternative) via Generically1 Tile

ewTile :: a -> Tile a
ewTile x = Tile (Just x) Nothing (Just x) Nothing

nsTile :: a -> Tile a
nsTile x = Tile Nothing (Just x) Nothing (Just x)

type Hole = Tile Color

type DigPlan = M.Map Coord Hole

data ScanState = ScSt {getSum :: Int, _above, _below, _inside :: Bool} deriving (Show)

data Segment = MkSeg {getLen :: Int, isVert :: Bool} deriving (Eq, Ord, Generic, NFData)

instance Show Segment where
  show (MkSeg l True) = "V-" <> show l
  show (MkSeg l False) = "H-" <> show l

type DigPlanLg = SQ.Seq (Coord, Segment)

type Slice = SQ.Seq (Coord, Segment, Bool)

-- * Parsing

dirP :: Parser Direction
dirP = eastP <|> northP <|> westP <|> southP
  where
    eastP = (mkCharParser '0' <|> mkCharParser 'R') $> East
    northP = (mkCharParser '1' <|> mkCharParser 'U') $> North
    westP = (mkCharParser '2' <|> mkCharParser 'L') $> West
    southP = (mkCharParser '3' <|> mkCharParser 'D') $> South

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

prepareInput :: Input -> [Instruction]
prepareInput = map (fromMaybe (error "parse failed") . tryParse instP)

colorToInstP :: Parser Instruction
colorToInstP = (\x y -> Inst y (read $ "0x" ++ x) "none") <$> mkTakeParser 5 <*> dirP

convertForPart2 :: [Instruction] -> [Instruction]
convertForPart2 = map (fromMaybe (error "part 2 conversion parse fail") . tryParse colorToInstP . getColor)

-- * Solution

processInst :: (Coord, DigPlan) -> Instruction -> (Coord, DigPlan)
processInst ((i, j), dp) (Inst d l col) =
  (,)
    (last cs)
    (foldl' (\acc c -> M.insertWith (<|>) c hole acc) dp cs)
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

processInstLg :: (Coord, [(Coord, Segment)]) -> Instruction -> (Coord, [(Coord, Segment)])
processInstLg ((i, j), dpl) (Inst d l _) =
  (snd endpoints, (uncurry min endpoints, segment) : dpl)
  where
    endpoints = case d of
      North -> ((i, j), (i - l, j))
      South -> ((i, j), (i + l, j))
      West -> ((i, j), (i, j - l))
      East -> ((i, j), (i, j + l))
    segment = MkSeg (l + 1) (d == North || d == South)

mkDigPlanLg :: [Instruction] -> DigPlanLg
mkDigPlanLg = SQ.fromList . reverse . snd . foldl' processInstLg (origin, [])
  where
    origin = (0, 0)

mkRowRanges :: DigPlanLg -> S.Set (Meter, Meter)
mkRowRanges = foldMap f . (SQ.zip <*> SQ.drop 1) . fdEndpointRows
  where
    fdEndpointRows = SQ.sort . fmap (fst . fst)
    f is@(i, i'next) =
      if i == i'next
        then S.singleton is
        else S.fromList [(i, i), (i + 1, i'next - 1), (i'next, i'next)]

fdAreaRanges :: DigPlanLg -> S.Set (Meter, Meter) -> Int
fdAreaRanges dpl =
  sum
    . map ((*) <$> countRows <*> countCols)
    . S.toList
  where
    countRows (x, y) = y - x + 1
    countCols (i'slice, _) = ((+) <$> colsIn <*> countWalls) slice
      where
        segCount = length dpl

        hitSegments =
          SQ.sortOn (\(_, (_, x), _) -> x)
            . SQ.foldMapWithIndex
              ( \idx c'seg@(c, seg) ->
                  if rowHitsSeg c'seg
                    then SQ.singleton (idx, c, seg)
                    else empty
              )
            $ dpl

        slice :: Slice
        slice =
          -- next line is just formatting, and could be made clearer but idrc at this point to clean it up
          (SQ.zipWith mk3 <$> fmap (\(_, c, seg) -> (c, seg)) <*> fmap isVerticalBoundary)
            . fmap sliceSegment
            $ hitSegments
          where
            mk3 (x, y) z = (x, y, z)
            sliceSegment i'c'seg@(dplIdx, (_, c'j), seg) =
              if isVert seg
                then (dplIdx, (i'slice, c'j), MkSeg 1 True)
                else i'c'seg

        -- the row being sliced needs to hit *inside* the segment, not just the ends
        rowHitsSeg ((i'seg, _), seg) =
          if isVert seg
            then i'seg < i'slice && i'slice < i'seg + getLen seg - 1
            else i'slice == i'seg

        isVerticalBoundary (dplIdx, (i'seg, _), MkSeg _ False) =
          let ((i'prev, _), _) = fromJust $ dpl SQ.!? ((dplIdx - 1) `mod` segCount)
              ((i'next, _), _) = fromJust $ dpl SQ.!? ((dplIdx + 1) `mod` segCount)
              segBelow = i'seg == i'prev || i'seg == i'next
              segAbove = i'prev < i'seg || i'next < i'seg
           in segBelow && segAbove
        isVerticalBoundary (_, _, MkSeg _ True) = True

        splitOuter s =
          let (l, m'r) = SQ.breakl isBound s
              (r, m) = SQ.breakr isBound m'r
           in (l, m, r)

        isBound (_, _, b) = b

        countWalls :: Slice -> Int
        countWalls = foldl' (\acc (_, s, _) -> acc + getLen s) 0

        -- columns inside walled regions, not including walls
        colsIn :: Slice -> Int
        colsIn s =
          case inner of
            Empty -> 0
            _ :<| Empty -> error "can't have only 1 region boundary, must be even"
            _ ->
              let (((_, j'l), seg'l, _) :<| si'r) = inner
                  (innerSlice :|> ((_, j'r), _, _)) = si'r
                  cols = j'r - (j'l + getLen seg'l)
               in cols - colsOut innerSlice
          where
            (_, inner, _) = splitOuter s

        -- \| columns outside walled regions, including the walls themselves
        colsOut :: Slice -> Int
        colsOut s =
          case inner of
            Empty -> outerCols
            _ :<| Empty -> error "can't have only 1 region boundary, must be even"
            _ ->
              let (((_, j'l), seg'l, _) :<| si'r) = inner
                  (innerSlice :|> ((_, j'r), seg'r, _)) = si'r
                  cols = j'r - (j'l + getLen seg'l)
               in cols + outerCols + getLen seg'l + getLen seg'r - colsIn innerSlice
          where
            (outerL, inner, outerR) = splitOuter s
            outerCols = countWalls (outerL >< outerR)

solutionPart2 :: Input -> Int
solutionPart2 =
  (fdAreaRanges <*> mkRowRanges)
    . mkDigPlanLg
    . convertForPart2
    . prepareInput

solveDay18 :: FilePath -> IO ()
solveDay18 input'path = do
  input'string <- readFile input'path

  let input'lines = takeWhile (not . lineEmpty) . lines $ input'string

  let s1 = solutionPart1 input'lines
  putStrLn $ "Solution to part 1: " <> show s1

  let s2 = solutionPart2 input'lines
  putStrLn $ "Solution to part 2: " <> show (s2 `deepseq` s2)
