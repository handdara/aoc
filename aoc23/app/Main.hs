module Main where

import qualified Aoc as A
import Control.Concurrent (getNumCapabilities, setNumCapabilities)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import GHC.Conc (getNumProcessors)
import Turtle hiding (fp)

dayParser :: Parser Int
dayParser = argInt "day" "which day to solve"

extraParser :: Parser [Text]
extraParser = many $ argText "extra" "extra inputs, needed for days: 8"

inputParser :: Parser FilePath
inputParser = optPath "input" 'i' "input file to use, default: \"input<DAY>.txt\""

outputParser :: Parser FilePath
outputParser = optPath "output" 'o' "output file if needed"

commandParser :: Parser A.Command
commandParser =
  subcommand "one" "solve day 1 of advent of code" (A.Day1 <$> optional inputParser)
    <|> subcommand "two" "solve day 2 of advent of code" (A.Day2 <$> optional inputParser)
    <|> subcommand "three" "solve day 3 of advent of code" (A.Day3 <$> optional inputParser)
    <|> subcommand "four" "solve day 4 of advent of code" (A.Day4 <$> optional inputParser)
    <|> subcommand "five" "solve day 5 of advent of code" (A.Day5 <$> optional inputParser)
    <|> subcommand "six" "solve day 6 of advent of code" (A.Day6 <$> optional inputParser)
    <|> subcommand "seven" "solve day 7 of advent of code" (A.Day7 <$> optional inputParser)
    <|> subcommand "eight" "solve day 8 of advent of code" (A.Day8 <$> extraParser <*> optional inputParser)
    <|> subcommand "nine" "solve day 9 of advent of code" (A.Day9 <$> optional inputParser)
    <|> subcommand "ten" "solve day 10 of advent of code" (A.Day10 <$> optional inputParser)
    <|> subcommand "eleven" "solve day 11 of advent of code" (A.Day11 <$> optional inputParser)
    <|> subcommand "twelve" "solve day 12 of advent of code" (A.Day12 <$> extraParser <*> optional inputParser)
    <|> subcommand "thirteen" "solve day 13 of advent of code" (A.Day13 <$> optional inputParser)
    <|> subcommand "fourteen" "solve day 14 of advent of code" (A.Day14 <$> extraParser <*> optional inputParser)
    <|> subcommand "fifteen" "solve day 15 of advent of code" (A.Day15 <$> optional inputParser)
    <|> subcommand "sixteen" "solve day 16 of advent of code" (A.Day16 <$> optional inputParser)
    <|> subcommand "seventeen" "solve day 17 of advent of code" (A.Day17 <$> optional inputParser)
    <|> subcommand "eighteen" "solve day 18 of advent of code" (A.Day18 <$> optional inputParser)
    <|> subcommand "nineteen" "solve day 19 of advent of code" (A.Day19 <$> optional inputParser)
    <|> subcommand "testing" "dummy testing command" (A.Testing <$> extraParser)

optsParser :: Parser A.Opts
optsParser =
  A.Opts
    <$> switch "verbose" 'v' "Verbose mode for more logs"
    <*> optional (optInt "threads" 'n' "Simultaneous thread count maximum, capped at one lesss than processor number")

parser :: Parser (A.Command, A.Opts)
parser = (,) <$> commandParser <*> optsParser

aoc :: (MonadIO io) => A.Command -> A.Opts -> io ()
aoc command _ = do
  case command of
    A.Testing [n] -> liftIO $ A.testing n
    A.Testing _ -> die "expecting a single extra input for testing function"
    A.Day1 Nothing -> liftIO $ A.solveDay1 "input1.txt"
    A.Day1 (Just fp) -> liftIO $ A.solveDay1 fp
    A.Day2 Nothing -> liftIO $ A.solveDay2 "input2.txt"
    A.Day2 (Just fp) -> liftIO $ A.solveDay2 fp
    A.Day3 Nothing -> liftIO $ A.solveDay3 "input3.txt"
    A.Day3 (Just fp) -> liftIO $ A.solveDay3 fp
    A.Day4 Nothing -> liftIO $ A.solveDay4 "input4.txt"
    A.Day4 (Just fp) -> liftIO $ A.solveDay4 fp
    A.Day5 Nothing -> liftIO $ A.solveDay5 "input5.txt"
    A.Day5 (Just fp) -> liftIO $ A.solveDay5 fp
    A.Day6 Nothing -> liftIO $ A.solveDay6 "input6.txt"
    A.Day6 (Just fp) -> liftIO $ A.solveDay6 fp
    A.Day7 Nothing -> liftIO $ A.solveDay7 "input7.txt"
    A.Day7 (Just fp) -> liftIO $ A.solveDay7 fp
    A.Day8 [] Nothing -> liftIO $ A.solveDay8 "AAA" "input8.txt"
    A.Day8 [n] (Just fp) -> liftIO $ A.solveDay8 (unpack n) fp
    A.Day8 _ (Just _) -> die "expecting a single extra input for day 8: the starting node"
    A.Day9 Nothing -> liftIO $ A.solveDay9 "input9.txt"
    A.Day9 (Just fp) -> liftIO $ A.solveDay9 fp
    A.Day10 Nothing -> liftIO $ A.solveDay10 "input10.txt"
    A.Day10 (Just fp) -> liftIO $ A.solveDay10 fp
    A.Day11 Nothing -> liftIO $ A.solveDay11 "input11.txt"
    A.Day11 (Just fp) -> liftIO $ A.solveDay11 fp
    A.Day12 [] fp'm -> liftIO $ do
      putStrLn "Warning: Using default fold number 1"
      case fp'm of
        Nothing -> A.solveDay12 "1" "input12.txt"
        Just fp -> A.solveDay12 "1" fp
    A.Day12 [fn] Nothing -> liftIO $ A.solveDay12 (unpack fn) "input12.txt"
    A.Day12 [fn] (Just fp) -> liftIO $ A.solveDay12 (unpack fn) fp
    A.Day12 _ _ -> die "expecting a single extra input for day 12: the fold number for records"
    A.Day13 Nothing -> liftIO $ A.solveDay13 "input13.txt"
    A.Day13 (Just fp) -> liftIO $ A.solveDay13 fp
    A.Day14 [] fp'm -> liftIO $ do
      putStrLn "Warning: Using default number of cycles: 1000000000"
      case fp'm of
        Nothing -> A.solveDay14 "1000000000" "input14.txt"
        Just fp -> A.solveDay14 "1000000000" fp
    A.Day14 [cs] Nothing -> liftIO $ A.solveDay14 (unpack cs) "input14.txt"
    A.Day14 [cs] (Just fp) -> liftIO $ A.solveDay14 (unpack cs) fp
    A.Day15 Nothing -> liftIO $ A.solveDay15 "input15.txt"
    A.Day15 (Just fp) -> liftIO $ A.solveDay15 fp
    A.Day16 Nothing -> liftIO $ A.solveDay16 "input16.txt"
    A.Day16 (Just fp) -> liftIO $ A.solveDay16 fp
    A.Day17 Nothing -> liftIO $ A.solveDay17 "input17.txt"
    A.Day17 (Just fp) -> liftIO $ A.solveDay17 fp
    A.Day18 Nothing -> liftIO $ A.solveDay18 "input18.txt"
    A.Day18 (Just fp) -> liftIO $ A.solveDay18 fp
    A.Day19 Nothing -> liftIO $ A.solveDay19 "input19.txt"
    A.Day19 (Just fp) -> liftIO $ A.solveDay19 fp
    _ -> echo "This day hasn't been started yet!"

main :: IO ()
main = do
  (command, opts) <- options "Wrapper app for advent of code 2023 solutions" parser

  initThreadCap <- getNumCapabilities
  numProcessors <- getNumProcessors
  let threadsRequested = fromMaybe initThreadCap $ A.threadCap opts
  setNumCapabilities $ min threadsRequested (numProcessors - 1)

  aoc command opts
