{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Aoc.Solve.Eight
  ( solveDay8,
  )
where

import Aoc.Parse
import Control.Applicative
import Control.Monad (replicateM, (<=<))
import Data.Functor (($>))
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M

-- * Types

type Input = String

type Label = String

data
  (Eq l, Ord l) =>
  Instruction l
  = GoLeft l
  | GoRight l
  deriving (Eq)

instance (Eq l, Ord l, Show l) => Show (Instruction l) where
  show (GoLeft x) = "L-" ++ show x
  show (GoRight x) = "R-" ++ show x

data (Eq l, Ord l) => Node l a = Node l a

instance (Show l, Eq l, Ord l, Show a) => Show (Node l a) where
  show (Node label val) =
    "{Node "
      <> show label
      <> ": "
      <> show val
      <> "}"

type MapNode = Node Label [Instruction Label]

type MapGraph = M.Map Label [Instruction Label]

type Direction = Instruction ()

data DesertMap = DesertMap [Direction] MapGraph deriving (Show)

-- * Parsing

labelP :: Parser Label
labelP = replicateM 3 genericCharParser

mapNodeP :: Parser MapNode
mapNodeP =
  Node
    <$> labelP
    <*> sequenceA [GoLeft <$> l, GoRight <$> r]
  where
    l = whiteParser *> mkCharParser '=' *> whiteParser *> mkCharParser '(' *> labelP
    r = mkCharParser ',' *> whiteParser *> labelP <* mkCharParser ')' <* whiteParser

instP :: Parser Direction
instP = lP <|> rP
  where
    lP = mkCharParser 'L' $> GoLeft ()
    rP = mkCharParser 'R' $> GoRight ()

nodeListToGraph :: [MapNode] -> MapGraph
nodeListToGraph = M.fromList . map (\(Node k v) -> (k, v))

mapGraphP :: Parser MapGraph
mapGraphP = nodeListToGraph <$> many (mapNodeP <* whiteParser)

desertMapP :: Parser DesertMap
desertMapP =
  DesertMap
    <$> (many instP <* whiteParser)
    <*> mapGraphP

parseInput :: String -> Maybe DesertMap
parseInput = return . snd <=< runParser desertMapP

-- * Solutions

fdNext :: Direction -> [Instruction Label] -> Label
fdNext _ [] = error "couldn't find next node"
fdNext (GoLeft ()) (GoLeft label'next : _) = label'next
fdNext (GoRight ()) (GoRight label'next : _) = label'next
fdNext d is = fdNext d (tail is)

searchMap :: MapGraph -> Label -> [Direction] -> [Label]
searchMap _ [] _ = error "searchMap: must start from a non-empty search list"
searchMap _ ls [] = [ls]
searchMap g l (d : ds) = l'next : searchMap g l'next ds
  where
    l'next = fdNext d (g ! l)

searchFromNode :: Label -> DesertMap -> [Label]
searchFromNode l (DesertMap ds g) = l : searchMap g l (cycle ds)

dropAfterNode :: (Foldable t, Eq a) => a -> t a -> [a]
dropAfterNode l = foldr (\x ys -> if x /= l then x : ys else [x]) []

maxNumberOfSearches :: Int
maxNumberOfSearches = 10 ^ (7 :: Int)

capMax :: Foldable t => t a -> Maybe (t a)
capMax xs
  | length xs < maxNumberOfSearches = Just xs
  | otherwise = Nothing

solutionPart1 :: Label -> Input -> Maybe [Label]
solutionPart1 startNode =
  capMax
    . take maxNumberOfSearches
    . dropAfterNode "ZZZ"
    . searchFromNode startNode
    <=< parseInput

endsWith :: (Eq a) => [a] -> a -> Bool
endsWith xs x = last xs == x

fdStartNodes :: MapGraph -> [Label]
-- fdStartNodes = tracer "start nodes: " . filter (endsWith 'A') . M.keys
fdStartNodes = filter (`endsWith` 'A') . M.keys

stepOne :: MapGraph -> Direction -> Label -> Label
stepOne g d l = fdNext d (g ! l)

stepTillEndsOnZ g ds startLabel = until p f (startLabel, ds, 0, [])
  where
    f (l, ds', s, hits) =
      let hits'new = if l `endsWith` 'Z' then (s, l) : hits else hits
          (d : ds'') = if null ds' then ds else ds'
       in ( stepOne g d l,
            ds'',
            s + 1,
            hits'new
          )
    p (_, _, _, hits) = not (null hits)

calcGhostSteps (DesertMap ds g) = map (stepTillEndsOnZ g ds) startNodes
  where
    startNodes = fdStartNodes g

solutionPart2 :: Input -> Maybe Integer
solutionPart2 =
  return
    . foldr1 lcm
    . map (fst . head . frt)
    . calcGhostSteps
    <=< parseInput
  where
    frt (_, _, _, z) = z

solveDay8 :: Input -> FilePath -> IO ()
solveDay8 startNode input'path = do
  input'string <- readFile input'path

  let answerPart1 = solutionPart1 startNode input'string
  putStrLn "Solution to part 1: " -- <> show answerPart1
  putStrLn $ "\tSteps taken = " <> show (subtract 1 . length <$> answerPart1)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'string)
