module Aoc
  ( module Aoc.App,
    solveFunc,
  )
where

import Aoc.App
import Aoc.Solve.One
import Turtle
import Prelude hiding (FilePath, log)

solveFunc :: (MonadIO io) => Int -> Maybe FilePath -> Maybe FilePath -> io ()
solveFunc 1 Nothing _ = liftIO $ solveDay1 "input1.txt"
solveFunc 1 (Just input'path) _ = liftIO $ solveDay1 input'path
solveFunc _ _ _ = die "not implemented"
