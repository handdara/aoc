module Aoc.Solve.Fifteen
  ( solveDay15,
  )
where

import Aoc.Parse
import Data.Char (isSpace, ord)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict ((!))
import Data.Maybe (fromJust)

-- * Types

type Input = String

type FocalLen = Int

data HMInst = RemoveLens | PlaceLens FocalLen deriving (Eq, Show)

type HMLabel = String

data HMStep = HMStep HMLabel HMInst deriving (Eq, Show)

data Lens = Lens HMLabel FocalLen

type LensBoxes = IM.IntMap [Lens]

-- * Parsing

hashStringP :: Parser [String]
hashStringP = mkSepByParser (mkCharParser ',') (mkSpanParser (/= ','))

hmLabelP :: Parser HMLabel
hmLabelP = mkSpanParser (`notElem` ("-=" :: [Char]))

hmInstP :: Parser HMInst
hmInstP = rmP <|> plP
  where
    plP = PlaceLens <$> (mkCharParser '=' *> intParser)
    rmP = mkCharParser '-' $> RemoveLens

hmStepP :: Parser HMStep
hmStepP = HMStep <$> hmLabelP <*> hmInstP

hmStepsP :: Parser [HMStep]
hmStepsP = mkSepByParser (mkCharParser ',') hmStepP

hashPrepare :: Input -> [String]
hashPrepare = snd . fromJust . runParser hashStringP . filter (/= '\n')

hashmapPrepare :: Input -> [HMStep]
hashmapPrepare = snd . fromJust . runParser hmStepsP . filter (not . isSpace)

-- * Solution

applyHASH :: String -> Int
applyHASH = foldl processChar 0
  where
    processChar v c = ((v + ord c) * 17) `mod` 256

solutionPart1 :: [Char] -> Int
solutionPart1 = sum . map applyHASH . hashPrepare

updateBox :: HMInst -> HMLabel -> [Lens] -> [Lens]
updateBox RemoveLens label lenses =
  case break (\(Lens label'box _) -> label'box == label) lenses of
    (before, _:after) -> before <> after
    (lens'all, []) -> lens'all
updateBox (PlaceLens fl) label lenses =
  case break (\(Lens label'box _) -> label'box == label) lenses of
    (before, _:after) -> before <> (Lens label fl : after)
    (lens'all, []) -> lens'all <> [Lens label fl]

processHMStep :: LensBoxes -> HMStep -> LensBoxes
processHMStep lbM (HMStep label inst) =
  let boxIdx = applyHASH label
      lenses = lbM ! boxIdx
      lenses'new = updateBox inst label lenses
  in IM.insert boxIdx lenses'new lbM

initLensBoxes :: LensBoxes
initLensBoxes = foldr (`IM.insert` []) IM.empty [0..255]

doHASHMAP :: [HMStep] -> LensBoxes
doHASHMAP = foldl processHMStep initLensBoxes

boxToFocusPwr :: (Int, [Lens]) -> Int
boxToFocusPwr (boxIdx, lenses) = (boxIdx + 1) * sum lensProds
  where
    lensProds = zipWith (\(Lens _ fl) lensNum -> lensNum*fl) lenses ([1..]::[Int])

solutionPart2 :: Input -> Int
solutionPart2 = sum . map boxToFocusPwr . IM.toList . doHASHMAP . hashmapPrepare

solveDay15 :: FilePath -> IO ()
solveDay15 input'path = do
  input'string <- readFile input'path

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'string)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'string)
