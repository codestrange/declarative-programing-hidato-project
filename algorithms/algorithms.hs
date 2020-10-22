module Algorithms
( generate
, solve
, solveAll
) where

import Structures
import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map

generate :: Matrix
generate = read "{. x x x x x x x x \n . 8 x x x x x x x \n . . 11 x x x x x x \n 29 . 10 . x x x x x \n 30 . . . . x x x x \n . 31  1 38  .  . x x x \n . 32 . . 39 41 . x x \n . . . 22 . . 42 . x \n . . . . . . . 44 45}" :: Matrix

stepMatrix :: Int -> Matrix -> Cell -> Map Int Cell -> [(Matrix, Cell)]
stepMatrix step m@(Matrix rs cs ma) prevCell map = if Map.notMember step map
                        then [
                              (Matrix rs cs (Set.insert cell ma), cell) | cell <- getAdjacents prevCell rs cs step,
                              let actCell = Set.elemAt (Set.findIndex cell $ ma) ma,
                              value actCell == 0
                        ]
                        else  let actCell = map Map.! step
                              in [(m, actCell) | isAdjacent actCell prevCell || value actCell == 1]

buildMap :: Matrix -> Map Int Cell
buildMap ma@(Matrix r c m) = Set.foldl (\acc cell -> Map.insert (value cell) cell acc) Map.empty m

-- solveRecursiveBFS :: [(Matrix, Int)] -> [Matrix]
-- solveRecursiveBFS [] = []
-- solveRecursiveBFS ((actualMatrix, step):queue) 
--       | isFinalMatrix actualMatrix = actualMatrix:solveRecursiveBFS queue
--       | otherwise                  = let toAdd = [ (matrix, step + 1) 
--                                                       | matrix <- stepMatrix step actualMatrix]
--                                      in solveRecursiveBFS (queue ++ toAdd)

solveRecursiveDFS :: Matrix -> Int -> Cell -> Int -> Map Int Cell -> [Matrix]
solveRecursiveDFS actualMatrix step prevCell obs map
      | step == obs + 1 = [actualMatrix]
      | otherwise = let toAdd = stepMatrix step actualMatrix prevCell map
                    in concat [ solveRecursiveDFS matrix (step + 1) prevCell obs map | (matrix, prevCell) <- toAdd ]

solveAll :: Matrix -> [Matrix]
solveAll m = solveRecursiveDFS m 1 (Cell 0 0 0) ((rows m) * (columns m)  - countObstacles m) $ buildMap m

solve :: Matrix -> Matrix
solve m = let solves = solveAll m
          in case solves of [] -> error "Solve not found"
                            (x: xs) -> x
