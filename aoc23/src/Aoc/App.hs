module Aoc.App
  ( -- * App Types
    Command (..),
    Opts (..),

    -- * Logging
    log,
  )
where

import Turtle
import Prelude hiding (log)

-- * App Types

-- | Type encompassing implemented commands and necessary inputs for them
data Command
  = Solve
    { -- | the answer word given by the user
      day :: Int
    }
  deriving (Show)

-- | Type encompassing the command line flags
data Opts = Opts
  { verbosity :: Bool
  }

-- * Logging

log :: (MonadIO io) => Opts -> Text -> io ()
log opts = when (verbosity opts) . stdout . return . unsafeTextToLine . ("LOG: " <>)

