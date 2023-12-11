{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Aoc.Solve.Eleven
  ( solveDay11,
    -- dbg
    Input,
    Space,
    Direction (..),
    AxisVal,
    Coord,
    Line (..),
    Galaxies,
    GalaxyMap (..),
    solutionPart1,
    solutionPart2,
  )
where
import Debug.Trace (trace)

-- * Types

type Input = [[Char]]

data Space = Empty | Galaxy
  deriving (Show, Eq)

data Direction = PlusX | PlusY
  deriving (Show, Eq)

type AxisVal = Int

type Coord = (AxisVal, AxisVal)

type Vector = (AxisVal, AxisVal)

type Range = (AxisVal, AxisVal)

data Line = Line Coord Direction
  deriving (Show)

type Galaxies = [Coord]

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
prepareInput i = GalaxyMap (0,x'max) (0,y'max) galaxies
  where
    x'max = (maximum . map length $ i) - 1
    y'max = length i - 1
    line'y'zip = zip (reverse i) [0..]
    foldLineYCoord acc (l',y') = acc <> parseLine y' l'
    galaxies = foldl foldLineYCoord [] line'y'zip

-- * Solutions

isEmpty :: Line -> Galaxies -> Bool
isEmpty (Line (_,y) PlusX) = not . any (\(_,y'g) -> y'g == y)
isEmpty (Line (x,_) PlusY) = not . any (\(x'g,_) -> x'g == x)

fdEmptyYLines :: GalaxyMap -> [Line]
fdEmptyYLines (GalaxyMap (x'min,x'max) _ gs) = foldl accumEmptyXLines [] [x'min..x'max]
  where
    accumEmptyXLines ls x =
      let l = Line (x,0) PlusY
      in if isEmpty l gs
        then l:ls
        else ls

fdEmptyXLines :: GalaxyMap -> [Line]
fdEmptyXLines (GalaxyMap _ (y'min,y'max) gs) = foldl accumEmptyYLines [] [y'min..y'max]
  where
    accumEmptyYLines ls y =
      let l = Line (0,y) PlusX
      in if isEmpty l gs
        then l:ls
        else ls

fdEmptyLines :: GalaxyMap -> [Line]
fdEmptyLines = (<>) <$> fdEmptyXLines <*> fdEmptyYLines

expandEmptyLines :: GalaxyMap -> [Line] -> GalaxyMap
expandEmptyLines = undefined

expandGalaxy :: GalaxyMap -> GalaxyMap
expandGalaxy = expandEmptyLines <*> fdEmptyLines

getGalaxyPairs :: GalaxyMap -> [(Coord, Coord)]
getGalaxyPairs (GalaxyMap _ _ gs) = do
  l <- gs
  r <- gs
  if l /= r
    then return (l,r)
    else []

diff :: Coord -> Coord -> Vector
diff (xl, yl) (xr, yr) = (xr - xl, yr - yl)

normL1 :: Vector -> AxisVal
normL1 (x, y) = abs x + abs y

sumDistances :: GalaxyMap -> AxisVal
sumDistances = sum . map (normL1 . uncurry diff) . getGalaxyPairs

solutionPart1 :: Input -> AxisVal
solutionPart1 = sumDistances . expandGalaxy . prepareInput

solutionPart2 :: Input -> String
solutionPart2 = const ("In Progress" :: String)

solveDay11 :: FilePath -> IO ()
solveDay11 input'path = do
  input'string <- readFile input'path

  let input'lines = lines input'string

  putStrLn "Input: "
  mapM_ putStrLn input'lines

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'lines)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'lines)
