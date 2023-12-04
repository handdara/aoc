module Aoc
  ( module Aoc.App,
    module Aoc.Solve.Four,
    solveFunc,
  )
where

import Aoc.App
import Aoc.Solve.One
import Aoc.Solve.Three
import Aoc.Solve.Four
import Aoc.Solve.Two
import Turtle
import Prelude hiding (FilePath, log)

solveFunc :: (MonadIO io) => Int -> Maybe FilePath -> Maybe FilePath -> io ()
solveFunc 1 Nothing _ = liftIO $ solveDay1 "input1.txt"
solveFunc 1 (Just input'path) _ = liftIO $ solveDay1 input'path
solveFunc 2 Nothing _ = liftIO $ solveDay2 "input2.txt"
solveFunc 2 (Just input'path) _ = liftIO $ solveDay2 input'path
solveFunc 3 Nothing _ = liftIO $ solveDay3 "input3.txt"
solveFunc 3 (Just input'path) _ = liftIO $ solveDay3 input'path
solveFunc 4 Nothing _ = liftIO $ solveDay4 "input4.txt"
solveFunc 4 (Just input'path) _ = liftIO $ solveDay4 input'path
solveFunc _ _ _ = die "Solution not implemented yet"
