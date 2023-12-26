{-# LANGUAGE TupleSections #-}

module Aoc.Solve.Sixteen
  ( solveDay16,
  )
where

-- import Data.Group

import Control.Parallel.Strategies
import Data.Array.IArray ((!))
import qualified Data.Array.IArray as A
import qualified Data.Set as S
import Data.Time.Clock (diffUTCTime, getCurrentTime)

-- * Types

type Input = [String]

data Tile = Empty | VSplit | HSplit | UMirror | DMirror
  deriving (Eq, Ord)

instance Show Tile where
  show = return . tileToChar

newtype Coord = Coord (Int, Int) deriving (Show, Eq, Ord, A.Ix)

instance Semigroup Coord where
  Coord (xl, yl) <> Coord (xr, yr) = Coord (xl + xr, yl + yr)

instance Monoid Coord where
  mempty = Coord (0, 0)

type Direction = Coord

east :: Direction
east = Coord (0, 1)

north :: Direction
north = Coord (-1, 0)

west :: Direction
west = Coord (0, -1)

south :: Direction
south = Coord (1, 0)

data BeamFront = BeamFront Direction Coord
  deriving (Show, Eq, Ord)

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
prepareInput [] = error "prepareInput: called with empty input"
prepareInput i = A.listArray (Coord (0, 0), maxIdx) (map charToTile . mconcat $ i)
  where
    maxIdx = Coord (-1, -1) <> Coord ((,) <$> length <*> length . head $ i)

-- * Solution

updateBeamFront :: Tile -> BeamFront -> [BeamFront]
updateBeamFront VSplit (BeamFront d c) | d == east || d == west = [BeamFront north (c <> north), BeamFront south (c <> south)]
updateBeamFront HSplit (BeamFront d c) | d == north || d == south = [BeamFront east (c <> east), BeamFront west (c <> west)]
updateBeamFront UMirror (BeamFront d c)
  | d == east = [BeamFront north (c <> north)]
  | d == north = [BeamFront east (c <> east)]
  | d == west = [BeamFront south (c <> south)]
  | d == south = [BeamFront west (c <> west)]
updateBeamFront DMirror (BeamFront d c)
  | d == east = [BeamFront south (c <> south)]
  | d == north = [BeamFront west (c <> west)]
  | d == west = [BeamFront north (c <> north)]
  | d == south = [BeamFront east (c <> east)]
updateBeamFront _ (BeamFront d c) = [BeamFront d (c <> d)]

inBoundsOf :: Coord -> MirrorMchn -> Bool
inBoundsOf (Coord (cx, cy)) mm = lbi <= cx && cx <= ubi && lbj <= cy && cy <= ubj
  where
    (Coord (lbi, lbj), Coord (ubi, ubj)) = A.bounds mm

stepBeams :: MirrorMchn -> ([BeamFront], S.Set BeamFront) -> ([BeamFront], S.Set BeamFront)
stepBeams mm (bfs, visited) =
  let bfs'valid = [b | b@(BeamFront _ c) <- bfs, c `inBoundsOf` mm, b `S.notMember` visited]
      bfs'coords = map (\(BeamFront _ c) -> c) bfs'valid
      tiles'valid = map (mm !) bfs'coords
      bfs'new = zip tiles'valid bfs'valid >>= uncurry updateBeamFront
      visited'new = visited `S.union` S.fromList bfs'valid
   in (bfs'new, visited'new)

-- | taking a mirror machine configuration and initial beam front, propagate it through the
-- mirror machine and find the set of all lit coordinates
propagateBeam :: MirrorMchn -> BeamFront -> S.Set Coord
propagateBeam mm bf =
  let x0 = ([bf], S.empty)
      xf = until (null . fst) (stepBeams mm) x0
   in S.map (\(BeamFront _ c) -> c) (snd xf)

solutionPart1 :: Input -> Int
solutionPart1 = S.size . (`propagateBeam` bf'init) . prepareInput
  where
    bf'init = BeamFront east mempty

mkInitBeams :: MirrorMchn -> [BeamFront]
mkInitBeams mm =
  [BeamFront south c | c <- northBorder]
    <> [BeamFront north c | c <- southBorder]
    <> [BeamFront west c | c <- eastBorder]
    <> [BeamFront east c | c <- westBorder]
  where
    (Coord (lbi, lbj), Coord (ubi, ubj)) = A.bounds mm
    is = [lbi .. ubi]
    js = [lbj .. ubj]
    northBorder = map (Coord . (lbi,)) js
    southBorder = map (Coord . (ubi,)) js
    westBorder = map (Coord . (,lbj)) is
    eastBorder = map (Coord . (,ubj)) is

solutionPart2 :: Input -> Int
solutionPart2 i = maximum litTiles
  where
    mm = prepareInput i
    litTiles =
      map (S.size . propagateBeam mm) (mkInitBeams mm)
        `using` parList rseq

solveDay16 :: FilePath -> IO ()
solveDay16 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  t1 <- getCurrentTime
  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
  t2 <- getCurrentTime

  let durPart1 = diffUTCTime t2 t1
  putStrLn $ "Part 2 Solution time: " <> show durPart1
