module Aoc.App
  ( -- * App Types
    Command (..),
    Opts (..),

    -- * Logging
    log,
  )
where

import Turtle
import Prelude hiding (FilePath, log)

-- * App Types

-- | Type encompassing implemented commands and necessary inputs for them
data Command
  = Day1 (Maybe FilePath)
  | Day2 (Maybe FilePath)
  | Day3 (Maybe FilePath)
  | Day4 (Maybe FilePath)
  | Day5 (Maybe FilePath)
  | Day6 (Maybe FilePath)
  | Day7 (Maybe FilePath)
  | Day8 [Text] (Maybe FilePath)
  | Day9 (Maybe FilePath)
  | Day10 (Maybe FilePath)
  | Day11 (Maybe FilePath)
  | Day12 [Text] (Maybe FilePath)
  | Day13 (Maybe FilePath)
  | Day14 [Text] (Maybe FilePath)
  | Day15 (Maybe FilePath)
  | Day16 (Maybe FilePath)
  | Day17 (Maybe FilePath)
  | Day18 (Maybe FilePath)
  | Day19 (Maybe FilePath)
  | Day20 [Text] (Maybe FilePath) (Maybe FilePath)
  | Day21 [Text] (Maybe FilePath) (Maybe FilePath)
  | Day22 [Text] (Maybe FilePath) (Maybe FilePath)
  | Day23 [Text] (Maybe FilePath) (Maybe FilePath)
  | Day24 [Text] (Maybe FilePath) (Maybe FilePath)
  | Day25 [Text] (Maybe FilePath) (Maybe FilePath)
  | Testing [Text]
  deriving (Show)

-- | Type encompassing the command line flags
data Opts = Opts
  { verbosity :: Bool,
    threadCap :: Maybe Int
  }

-- * Logging

log :: (MonadIO io) => Opts -> Text -> io ()
log opts = when (verbosity opts) . stdout . return . unsafeTextToLine . ("LOG: " <>)
