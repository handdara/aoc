{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Aoc.Solve.Nineteen
  ( solveDay19,
    -- dbg
    Input,
    solutionPart1,
    solutionPart2,
  )
where

import Aoc.Parse
import Data.Char (isAlpha)
import qualified Data.List as L
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

-- * Types

type Input = String

type WLabel = String

type Condition = Part -> Bool

data Rule = Rule {_cond :: Condition, _dest :: WLabel}

instance Show Rule where
  show (Rule _ d) = "R-" ++ d

-- data Workflow = Workflow {_name :: WLabel, _rules :: [Rule]} deriving (Show)
type Workflow = (WLabel, [Rule])

data Part = Part {_x, _m, _a, _s :: Int} deriving (Show)

type Workflows = M.Map WLabel [Rule]

-- * Parsing

ineqP = lessThanP <|> greaterThanP
  where
    greaterThanP = (<) <$> (mkCharParser '>' *> intParser)
    lessThanP = (>) <$> (mkCharParser '<' *> intParser)

condP = xCondP <|> mCondP <|> aCondP <|> sCondP
  where
    xCondP = (. _x) <$> (mkCharParser 'x' *> ineqP)
    mCondP = (. _m) <$> (mkCharParser 'm' *> ineqP)
    aCondP = (. _a) <$> (mkCharParser 'a' *> ineqP)
    sCondP = (. _s) <$> (mkCharParser 's' *> ineqP)

ruleP :: Parser Rule
ruleP = hasIneqRuleP <|> noIneqRuleP
  where
    hasIneqRuleP = Rule <$> condP <*> (mkCharParser ':' *> mkSpanParser isAlpha)
    noIneqRuleP = Rule (const True) <$> mkSpanParser isAlpha

rulesP :: Parser [Rule]
rulesP =
  mkCharParser '{'
    *> mkSepByParser (mkCharParser ',') ruleP
    <* mkCharParser '}'

workflowP :: Parser Workflow
workflowP = (,) <$> mkSpanParser (/= '{') <*> rulesP

partP :: Parser Part
partP =
  Part
    <$> (mkStringParser "{x=" *> intParser)
    <*> (mkStringParser ",m=" *> intParser)
    <*> (mkStringParser ",a=" *> intParser)
    <*> (mkStringParser ",s=" *> intParser <* mkCharParser '}')

newlineP = mkCharParser '\n'

-- workflowsPartsP :: Parser ([Workflow], [Part])
workflowsPartsP = (,) <$> workflowsP <*> partsP
  where
    workflowsP = M.fromList <$> mkSepByParser newlineP workflowP
partsP = whiteParser *> mkSepByParser newlineP partP

-- * Solutions

-- testPart :: Workflows -> Part -> Bool
testPart wfM p = case until (`elem` ["R","A"]) fdNextWF "in" of
  "R" -> False
  "A" -> True
  res -> error $ "unexpected result in testPart: " <> res
  where
    testRule :: Rule -> Bool
    testRule (Rule cond _) = cond p

    fdNextWF :: WLabel -> WLabel
    fdNextWF l = case L.find testRule (wfM ! l) of
      Just (Rule _ l') -> l'
      Nothing -> error "couldn't find rule"

solutionPart1 i = sum . map sumPartRatings . filter (testPart wfM) $ ps
  where
    (wfM, ps) = fromJust . tryParse workflowsPartsP $ i
    sumPartRatings (Part x m a s) = x + m + a + s

solutionPart2 :: Input -> String
solutionPart2 = const ("In Progress" :: String)

solveDay19 :: FilePath -> IO ()
solveDay19 input'path = do
  input'string <- readFile input'path

  -- putStrLn "Input: "
  -- putStrLn input'string

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'string)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'string)
