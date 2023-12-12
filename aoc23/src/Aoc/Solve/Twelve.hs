{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Aoc.Solve.Twelve
  ( solveDay12,
    -- dbg
    Input,
    solutionPart1,
    solutionPart2,
  )
where

import Aoc.Parse
import Control.Parallel
import Control.Parallel.Strategies
import Debug.Trace (trace)
import Control.DeepSeq (deepseq)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- * Types

type Input = String

data Status = OK | Broken | Unknown deriving (Eq, Ord)

instance Show Status where
  show OK = "✓"
  show Broken = "X"
  show Unknown = "?"

type StatusList = [Status]

type Group = Int

data RecordRow = RecordRow StatusList [Group]

instance Show RecordRow where
  show (RecordRow ss gs) = 
    "{ " 
      <> foldMap show ss
      <> " : "
      <> show gs
      <> " }"

type SpringRecord = [RecordRow]

-- * Parsing

okP :: Parser Status
okP = mkCharParser '.' $> OK

brokenP :: Parser Status
brokenP = mkCharParser '#' $> Broken

unknownP :: Parser Status
unknownP = mkCharParser '?' $> Unknown

entryP :: Parser Status
entryP = okP <|> brokenP <|> unknownP

entriesP :: Parser [Status]
entriesP = some entryP

groupsP :: Parser [Group]
groupsP = mkSepByParser (mkCharParser ',') intParser

rowP :: Parser RecordRow
rowP =
  RecordRow
    <$> (entriesP <* whiteParser)
    <*> groupsP

springRecordP :: Parser SpringRecord
springRecordP = mkSepByParser (mkCharParser '\n') rowP

parseInput = return . snd <=< runParser springRecordP

-- * Solutions

invalid :: [Group] -> StatusList -> Bool
invalid gs ss = undefined

tracer msg x = trace (msg<>show x) x

fdPossibleConfigs :: [Group] -> StatusList -> [StatusList]
fdPossibleConfigs gs sl
  | invalid gs sl = []
  | otherwise = case break (==Unknown) sl of
    (_,[]) -> return sl
    (gs'known,_:gs'rest) -> do
      sl'ok <- fdPossibleConfigs gs ( gs'known <> (OK:gs'rest) )
      sl'broken <- fdPossibleConfigs gs ( gs'known <> (Broken:gs'rest) )
      [sl'ok, sl'broken]

countPossible :: [Group] -> StatusList -> Int
countPossible gs sl
  -- | invalid gs sl = 0
  | otherwise = case break (==Unknown) sl of
    (_,[]) -> 1
    (gs'known,_:gs'rest) -> 
      let sl'ok = countPossible gs ( gs'known <> (OK:gs'rest) )
          sl'broken = countPossible gs ( gs'known <> (Broken:gs'rest) )
      in sl'ok `deepseq` sl'ok + sl'broken

countAll :: SpringRecord -> Integer
countAll = sum . map (fromIntegral . unwrapAndCount)
  where
    unwrapAndCount (RecordRow sl gs) = countPossible gs sl

countAll'parallel :: SpringRecord -> Integer
countAll'parallel rows = results `deepseq` sum results
  where
    results = 
      map (fromIntegral . \(RecordRow sl gs) -> countPossible gs sl) rows 
        `using` parList rdeepseq

solutionPart1 :: Input -> Maybe Integer
solutionPart1 = return . countAll <=< parseInput

solutionPart1'parallel :: Input -> Maybe Integer
solutionPart1'parallel = return . countAll'parallel <=< parseInput

solutionPart2 :: Input -> String
solutionPart2 = const ("In Progress" :: String)

solveDay12 :: FilePath -> IO ()
solveDay12 input'path = do
  input'string <- readFile input'path

  -- putStrLn "Input: "
  -- putStrLn input'string

  -- t0 <- getCurrentTime
  -- putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'string)
  t1 <- getCurrentTime
  putStrLn $ "Solution to part 1 (parallel): " <> show (solutionPart1'parallel input'string)
  t2 <- getCurrentTime

  -- let dur'single = diffUTCTime t1 t0 
  -- putStrLn $ "\nSingle Thread Solution time: " <> show dur'single

  let dur'par = diffUTCTime t2 t1 
  putStrLn $ "Parallel Solution time: " <> show dur'par

  putStrLn $ "\nSolution to part 2: " <> show (solutionPart2 input'string)
