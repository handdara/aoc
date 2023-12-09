module Main where

import Turtle
import qualified Aoc as A

dayParser :: Parser Int
dayParser = argInt "day" "which day to solve"

extraParser :: Parser [Text]
extraParser = many $ argText "extra" "extra inputs, needed for days: 8"

inputParser :: Parser FilePath
inputParser = optPath "input" 'i' "input file to use, default: \"input<DAY>.txt\""

outputParser :: Parser FilePath
outputParser = optPath "output" 'o' "output file if needed"

solveParser :: Parser A.Command
solveParser = 
  A.Solve 
    <$> dayParser
    <*> extraParser
    <*> optional inputParser
    <*> optional outputParser

commandParser :: Parser A.Command
commandParser = 
  subcommand "solve" "solve a day of advent of code" solveParser

optsParser :: Parser A.Opts
optsParser =
  A.Opts
    <$> switch "verbose" 'v' "Verbose mode for more logs"

parser :: Parser (A.Command, A.Opts)
parser = (,) <$> commandParser <*> optsParser

aoc :: (MonadIO io) => A.Command -> A.Opts -> io ()
aoc command _ = do
  case command of
    A.Solve day extras in'm out'm -> A.solveFunc day extras in'm out'm 

main :: IO ()
main = do
  (command, opts) <- options "Wrapper app for advent of code 2023 solutions" parser

  aoc command opts
