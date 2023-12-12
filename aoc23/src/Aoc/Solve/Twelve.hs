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

-- * Types

type Input = String

data Entry = Empty | Spring deriving (Show, Eq, Ord)

type Group = Int

data RecordRow = RecordRow [Maybe Entry] [Group]
  deriving (Show)

type SpringRecord = [RecordRow]

-- * Parsing

emptyP :: Parser Entry
emptyP = mkCharParser '.' $> Empty

springP :: Parser Entry
springP = mkCharParser '#' $> Spring

nothingP :: Parser (Maybe Entry)
nothingP = mkCharParser '?' $> Nothing

entryP :: Parser (Maybe Entry)
entryP =
  (Just <$> emptyP)
    <|> (Just <$> springP)
    <|> nothingP

entriesP :: Parser [Maybe Entry]
entriesP = some entryP

groupP :: Parser Group
groupP = intParser

groupsP :: Parser [Group]
groupsP = mkSepByParser (mkCharParser ',') groupP

rowP :: Parser RecordRow
rowP =
  RecordRow
    <$> (entriesP <* whiteParser)
    <*> groupsP

springRecordP :: Parser SpringRecord
springRecordP = mkSepByParser (mkCharParser '\n') rowP

parseInput = return . snd <=< runParser springRecordP

-- * Solutions

solutionPart1 :: Input -> Maybe SpringRecord
solutionPart1 = parseInput

solutionPart2 :: Input -> String
solutionPart2 = const ("In Progress" :: String)

solveDay12 :: FilePath -> IO ()
solveDay12 input'path = do
  input'string <- readFile input'path

  putStrLn "Input: "
  putStrLn input'string

  putStrLn $ "Solution to part 1: " <> show (solutionPart1 input'string)

  putStrLn $ "Solution to part 2: " <> show (solutionPart2 input'string)
