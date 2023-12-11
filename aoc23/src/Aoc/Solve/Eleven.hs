module Aoc.Solve.Eleven
  ( solveDay11,
  )
where

import qualified Data.List as L

-- * Types

type Input = [[Char]]

data Space = Empty | Galaxy
  deriving (Show, Eq)

data Direction = PlusX | PlusY
  deriving (Show, Eq, Ord)

type AxisVal = Integer

type Coord = (AxisVal, AxisVal)

type Vector = (AxisVal, AxisVal)

type Range = (AxisVal, AxisVal)

data Line = Line Coord Direction
  deriving (Show, Eq)

instance Ord Line where
  (Line cl dl) `compare` (Line cr dr) =
    case compare dl dr of
      EQ -> compare cl cr
      res -> res

type Galaxies = [Coord]

data Segment = Segment Coord Coord
  deriving (Show)

instance Eq Segment where
  (Segment lcl lcr) == (Segment rcl rcr) =
    min lcl lcr == min rcl rcr
      && max lcl lcr == max rcl rcr

instance Ord Segment where
  compare sl sr = case compare (getMin sl) (getMin sr) of
    EQ -> compare (getMax sl) (getMax sr)
    res -> res
    where
      getMin (Segment cl cr) = min cl cr
      getMax (Segment cl cr) = max cl cr

data GalaxyMap = GalaxyMap Range Range Galaxies
  deriving (Show)

-- * Parsing

parseLine :: AxisVal -> [Char] -> Galaxies
parseLine y cs = foldl foldCharXCoord [] char'x'zip
  where
    char'x'zip = zip cs [0 ..]
    foldCharXCoord acc (c', x') = case c' of
      '#' -> (x', y) : acc
      _ -> acc

prepareInput :: Input -> GalaxyMap
prepareInput i = GalaxyMap (0, fromIntegral x'max) (0, fromIntegral y'max) galaxies
  where
    x'max = (maximum . map length $ i) - 1
    y'max = length i - 1
    line'y'zip = zip (reverse i) [0 ..]
    foldLineYCoord acc (l', y') = acc <> parseLine y' l'
    galaxies = foldl foldLineYCoord [] line'y'zip

-- renderGM :: GalaxyMap -> Input
-- renderGM (GalaxyMap (xl, xh) (yl, yh) gs) =
--   reverse
--     [ [mkCh (x, y) | x <- [xl .. xh]]
--       | y <- [yl .. yh]
--     ]
--   where
--     mkCh c = if c `elem` gs then '#' else '.'

-- * Solutions

isEmpty :: Line -> Galaxies -> Bool
isEmpty (Line (_, y) PlusX) = not . any (\(_, y'g) -> y'g == y)
isEmpty (Line (x, _) PlusY) = not . any (\(x'g, _) -> x'g == x)

fdEmptyYLines :: GalaxyMap -> [Line]
fdEmptyYLines (GalaxyMap (x'min, x'max) _ gs) = foldl accumEmptyXLines [] [x'min .. x'max]
  where
    accumEmptyXLines ls x =
      let l = Line (x, 0) PlusY
       in if isEmpty l gs
            then l : ls
            else ls

fdEmptyXLines :: GalaxyMap -> [Line]
fdEmptyXLines (GalaxyMap _ (y'min, y'max) gs) = foldl accumEmptyYLines [] [y'min .. y'max]
  where
    accumEmptyYLines ls y =
      let l = Line (0, y) PlusX
       in if isEmpty l gs
            then l : ls
            else ls

fdEmptyLines :: GalaxyMap -> [Line]
fdEmptyLines =
    L.sort
    . ((<>) <$> fdEmptyXLines <*> fdEmptyYLines)

expandLine :: AxisVal -> Line -> GalaxyMap -> GalaxyMap
expandLine n (Line (x, y) d) (GalaxyMap xr@(x'min, x'max) yr@(y'min, y'max) gs) =
  case d of
    PlusX -> GalaxyMap xr (y'min, y'max + n - 1) (gs'idX <> gs'incX)
    PlusY -> GalaxyMap (x'min, x'max + n - 1) yr (gs'idY <> gs'incY)
  where
    gs'idX = [c | c@(_, y'g) <- gs, y'g <= y]
    gs'incX = [(x'g, y'g + n - 1) | (x'g, y'g) <- gs, y < y'g]
    gs'idY = [c | c@(x'g, _) <- gs, x'g <= x]
    gs'incY = [(x'g + n - 1, y'g) | (x'g, y'g) <- gs, x < x'g]

-- | The right fold is important because the lines farther from the origin
-- must be expanded first. Doing it in this order ensures that lines do not
-- need to shift other lines when expanding
expandEmptyLines :: AxisVal -> GalaxyMap -> [Line] -> GalaxyMap
expandEmptyLines n = foldr (expandLine n)

expandGalaxy :: AxisVal -> GalaxyMap -> GalaxyMap
expandGalaxy n = expandEmptyLines n <*> fdEmptyLines

getGalaxyPairs :: GalaxyMap -> [Segment]
getGalaxyPairs (GalaxyMap _ _ gs) =
  [Segment l r | l <- gs, r <- gs, l < r]

diff :: Coord -> Coord -> Vector
diff (xl, yl) (xr, yr) = (xr - xl, yr - yl)

normL1 :: Vector -> AxisVal
normL1 (x, y) = abs x + abs y

sumDistances :: GalaxyMap -> AxisVal
sumDistances =
  sum
    . map (normL1 . (\(Segment l r) -> diff l r))
    . getGalaxyPairs

expansionFactor'part1 :: AxisVal
expansionFactor'part1 = 2

solutionPart1 :: Input -> AxisVal
solutionPart1 =
  sumDistances
    . expandGalaxy expansionFactor'part1
    . prepareInput

expansionFactor'part2 :: AxisVal
expansionFactor'part2 = 10 ^ (6 :: Integer)

solutionPart2 :: Input -> AxisVal
solutionPart2 =
  sumDistances
    . expandGalaxy expansionFactor'part2
    . prepareInput

solveDay11 :: FilePath -> IO ()
solveDay11 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
