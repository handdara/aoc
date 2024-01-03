{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Aoc.Solve.Nineteen
  ( solveDay19,
  )
where

import Aoc.Parse
import Data.Foldable (toList)
import qualified Data.List as L
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

-- * Types

-- ** Part 1 Types

type Input = String

type WLabel = String

data Rating = X | M | A | S deriving (Show)

data Condition = Condition {_rating :: Rating, _compType :: Ordering, _compval :: Int}

data Rule = Rule {_cond :: Condition, _dest :: WLabel}

instance Show Rule where show (Rule _ d) = "R-" ++ d

type Workflow = (WLabel, [Rule])

data Part a = Part {getX, getM, getA, getS :: a} deriving (Functor, Foldable)

instance (Show a) => Show (Part a) where show p = "{" ++ (L.intercalate "," . toList $ show <$> p) ++ "}"

type SinglePart = Part Int

type Workflows = M.Map WLabel [Rule]

-- ** Part 2 Types

data Range a = MkRg {_lo, _hi :: a} deriving (Eq, Ord, Functor, Foldable)

instance (Show a) => Show (Range a) where show (MkRg lo hi) = "[" ++ show lo ++ ".." ++ show hi ++ "]"

type RatingRange = Range Int

type PartSet = Part RatingRange

data PSState = St {getStateLabel :: WLabel, getStatePartSet :: PartSet}

data Split a = Together a | Split a a deriving (Show, Foldable)

-- * Parsing

condP :: Parser Condition
condP = Condition <$> ratingP <*> ineqP <*> intParser
  where
    ratingP = (mkCharParser 'x' $> X) <|> (mkCharParser 'm' $> M) <|> (mkCharParser 'a' $> A) <|> (mkCharParser 's' $> S)
    ineqP = lessThanP <|> greaterThanP
      where
        greaterThanP = mkCharParser '>' $> GT
        lessThanP = mkCharParser '<' $> LT

ruleP :: Parser Rule
ruleP = hasIneqRuleP <|> noIneqRuleP
  where
    hasIneqRuleP = Rule <$> condP <*> (mkCharParser ':' *> mkSpanParser isAlpha)
    noIneqRuleP = Rule (Condition X GT 0) <$> mkSpanParser isAlpha

rulesP :: Parser [Rule]
rulesP =
  mkCharParser '{'
    *> mkSepByParser (mkCharParser ',') ruleP
    <* mkCharParser '}'

workflowP :: Parser Workflow
workflowP = (,) <$> mkSpanParser isAlpha <*> rulesP

singlePartP :: Parser SinglePart
singlePartP =
  Part
    <$> (mkStringParser "{x=" *> intParser)
    <*> (mkStringParser ",m=" *> intParser)
    <*> (mkStringParser ",a=" *> intParser)
    <*> (mkStringParser ",s=" *> intParser <* mkCharParser '}')

workflowsPartsP :: Parser (Workflows, [SinglePart])
workflowsPartsP = (,) <$> workflowsP <*> partsP
  where
    newlineP = mkCharParser '\n' $> ()
    workflowsP = M.fromList <$> mkSepByParser newlineP workflowP
    partsP = whiteParser *> mkSepByParser newlineP singlePartP

-- * Solutions

-- ** Part 1

mkComp :: Condition -> SinglePart -> Bool
mkComp (Condition X ord v) = (== ord) . (`compare` v) . getX
mkComp (Condition M ord v) = (== ord) . (`compare` v) . getM
mkComp (Condition A ord v) = (== ord) . (`compare` v) . getA
mkComp (Condition S ord v) = (== ord) . (`compare` v) . getS

solutionPart1 :: String -> Int
solutionPart1 i = sum . map sum . filter testPart $ ps
  where
    (wfM, ps) = fromJust . tryParse workflowsPartsP $ i
    testPart p = case until (`elem` ["R", "A"]) fdNextWF "in" of
      "R" -> False
      "A" -> True
      res -> error $ "unexpected result in testPart: " <> res
      where
        testRule (Rule cond _) = mkComp cond p

        fdNextWF l = case L.find testRule (wfM ! l) of
          Just (Rule _ l') -> l'
          Nothing -> error "couldn't find rule"

-- ** Part 2

calcFinalRgs :: Workflows -> [PSState]
calcFinalRgs wfM = until allAtEnd (>>= toList . stepPartSet) initSts
  where
    initSts = [St "in" (Part initRg initRg initRg initRg)]
      where
        initRg = MkRg 1 4000

    allAtEnd = all ((`elem` ["R", "A"]) . getStateLabel)

    stepPartSet s@(St "A" _) = Together s
    stepPartSet s@(St "R" _) = Together s
    stepPartSet (St l ps@(Part xRg mRg aRg sRg)) =
      let rules = wfM ! l
          Rule (Condition condRating ord v) l'next = fromJust $ L.find applicable rules
          rg@(MkRg lo hi) = getRg condRating
          loSt = case ord of
            LT -> St l'next (replaceRg condRating (MkRg lo (v - 1)))
            GT -> St l (replaceRg condRating (MkRg lo v))
            EQ -> error "not using EQ currently"
          hiSt = case ord of
            LT -> St l (replaceRg condRating (MkRg v hi))
            GT -> St l'next (replaceRg condRating (MkRg (v + 1) hi))
            EQ -> error "not using EQ currently"
       in if v `inRange` rg
            then Split loSt hiSt
            else Together (St l'next ps)
      where
        applicable (Rule (Condition r ord v) _) = any ((== ord) . (`compare` v)) (getRg r)
        getRg X = xRg
        getRg M = mRg
        getRg A = aRg
        getRg S = sRg
        replaceRg X rg = Part rg mRg aRg sRg
        replaceRg M rg = Part xRg rg aRg sRg
        replaceRg A rg = Part xRg mRg rg sRg
        replaceRg S rg = Part xRg mRg aRg rg
        inRange v (MkRg l' u') = l' <= v && v <= u'

psCardinality :: PartSet -> Integer
psCardinality = product . fmap rgCardinality
  where
    rgCardinality (MkRg lo hi) = fromIntegral $ hi - lo + 1

solutionPart2 :: Input -> Integer
solutionPart2 =
  sum
    . map (psCardinality . getStatePartSet)
    . filter ((== "A") . getStateLabel)
    . calcFinalRgs
    . prepareInput
  where
    prepareInput = fst . fromJust . tryParse workflowsPartsP

solveDay19 :: FilePath -> IO ()
solveDay19 input'path = do
  input'string <- readFile input'path

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'string)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'string)
