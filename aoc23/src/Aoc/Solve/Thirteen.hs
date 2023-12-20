module Aoc.Solve.Thirteen
  ( solveDay13,
  )
where

import Aoc.Parse
import qualified Data.List as L

-- * Types

type Input = String

data AshRock = Ash | Rock deriving (Show, Eq)

type AshMap = [[AshRock]]

-- * Parsing

charToAR :: Char -> AshRock
charToAR '.' = Ash
charToAR '#' = Rock
charToAR c = error $ "unexpected char: " <> [c]

mkAshMap :: [String] -> AshMap
mkAshMap = map (map charToAR)

newlineP :: Parser ()
newlineP = mkCharParser '\n' $> ()

rowP :: Parser String
rowP = ensureNonempty $ mkSpanParser (/= '\n') <* newlineP

ashMapsP :: Parser [AshMap]
ashMapsP = map mkAshMap <$> mkSepByParser newlineP (some rowP)

prepareInput :: Input -> Maybe [AshMap]
prepareInput = return . snd <=< runParser ashMapsP

-- * Solutions

cntAsMirrorBeforeRow :: AshMap -> Int -> Int
cntAsMirrorBeforeRow a i = sum $ zipWith cntDiff (reverse a'l) a'r
  where
    (a'l,a'r) = splitAt i a
    cntDiff xs ys = length . filter id $ zipWith (/=) xs ys

fdRowBeforeMirror :: Int -> AshMap -> Maybe Int
fdRowBeforeMirror smudges a = L.elemIndex smudges . map (cntAsMirrorBeforeRow a) $ [1..length a - 1]

fdMirrors :: Int -> AshMap -> (Maybe Int, Maybe Int)
fdMirrors smudges = (,) <$> fdRowBeforeMirror smudges <*> fdRowBeforeMirror smudges . L.transpose

mkSummary :: (Maybe Int, Maybe Int) -> Int
mkSummary (Just i, Nothing) = 100*i + 100
mkSummary (Nothing, Just j) = j + 1
mkSummary (Just _, Just _) = error "both found"
mkSummary (Nothing, Nothing) = error "nothing found"

solutionPart1 :: Input -> Maybe Int
solutionPart1 = return . sum . map (mkSummary . fdMirrors 0) <=< prepareInput

solutionPart2 :: Input -> Maybe Int
solutionPart2 = return . sum . map (mkSummary . fdMirrors 1) <=< prepareInput

solveDay13 :: FilePath -> IO ()
solveDay13 input'path = do
  input'string <- readFile input'path

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'string)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'string)
