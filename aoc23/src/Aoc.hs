module Aoc
  ( module Aoc.App,
    solveFunc,
  )
where

import Aoc.App
import Aoc.Solve.One
import Aoc.Solve.Three
import Aoc.Solve.Two
import Turtle
import Prelude hiding (FilePath, log)

solveFunc :: (MonadIO io) => Int -> Maybe FilePath -> Maybe FilePath -> io ()
solveFunc 1 Nothing _ = die "Must give input file"
solveFunc 1 (Just input'path) _ = liftIO $ solveDay1 input'path
solveFunc 2 Nothing _ = die "Must give input file"
solveFunc 2 (Just input'path) _ = liftIO $ solveDay2 input'path
solveFunc 3 Nothing _ = die "Must give input file"
solveFunc 3 (Just input'path) _ = liftIO $ solveDay3 input'path
solveFunc _ _ _ = die "not implemented"
