module Aoc
  ( module Aoc.App,
    module Aoc.Solve.Twelve,
    solveFunc,
  )
where

import Aoc.App
import Aoc.Solve.Eight
import Aoc.Solve.Five
import Aoc.Solve.Four
import Aoc.Solve.Nine
import Aoc.Solve.One
import Aoc.Solve.Seven
import Aoc.Solve.Six
import Aoc.Solve.Three
import Aoc.Solve.Two
import Aoc.Solve.Ten
import Aoc.Solve.Eleven
import Aoc.Solve.Twelve
import Data.Text (unpack)
import Turtle
import Prelude hiding (FilePath, log)

solveFunc :: (MonadIO io) => Int -> [Text] -> Maybe FilePath -> Maybe FilePath -> io ()
solveFunc 1 _ Nothing _ = liftIO $ solveDay1 "input1.txt"
solveFunc 1 _ (Just input'path) _ = liftIO $ solveDay1 input'path
solveFunc 2 _ Nothing _ = liftIO $ solveDay2 "input2.txt"
solveFunc 2 _ (Just input'path) _ = liftIO $ solveDay2 input'path
solveFunc 3 _ Nothing _ = liftIO $ solveDay3 "input3.txt"
solveFunc 3 _ (Just input'path) _ = liftIO $ solveDay3 input'path
solveFunc 4 _ Nothing _ = liftIO $ solveDay4 "input4.txt"
solveFunc 4 _ (Just input'path) _ = liftIO $ solveDay4 input'path
solveFunc 5 _ Nothing _ = liftIO $ solveDay5 "input5.txt"
solveFunc 5 _ (Just input'path) _ = liftIO $ solveDay5 input'path
solveFunc 6 _ Nothing _ = liftIO $ solveDay6 "input6.txt"
solveFunc 6 _ (Just input'path) _ = liftIO $ solveDay6 input'path
solveFunc 7 _ Nothing _ = liftIO $ solveDay7 "input7.txt"
solveFunc 7 _ (Just input'path) _ = liftIO $ solveDay7 input'path
solveFunc 8 [] Nothing _ = liftIO $ solveDay8 "AAA" "input8.txt"
solveFunc 8 [startNode] (Just input'path) _ = liftIO $ solveDay8 (unpack startNode) input'path
solveFunc 8 _ Nothing _ = die "expecting at least one extra input for day 8: starting node"
solveFunc 9 _ Nothing _ = liftIO $ solveDay9 "input9.txt"
solveFunc 9 _ (Just input'path) _ = liftIO $ solveDay9 input'path
solveFunc 10 _ Nothing _ = liftIO $ solveDay10 "input10.txt"
solveFunc 10 _ (Just input'path) _ = liftIO $ solveDay10 input'path
solveFunc 11 _ Nothing _ = liftIO $ solveDay11 "input11.txt"
solveFunc 11 _ (Just input'path) _ = liftIO $ solveDay11 input'path
solveFunc 12 _ Nothing _ = liftIO $ solveDay12 "input12.txt"
solveFunc 12 _ (Just input'path) _ = liftIO $ solveDay12 input'path
solveFunc _ _ _ _ = die "Solution not implemented yet"
