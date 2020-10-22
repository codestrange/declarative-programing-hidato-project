module Algorithms
( generate
, solve
, solveAll
, generateRandom
) where

import System.Random
import Structures
import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map

genRCell :: Int -> Int -> Int -> IO Cell
genRCell rn cn val = do
                              r <- randomRIO (1, rn)
                              c <- randomRIO (1, cn)
                              return (Cell r c val)


generateRandom :: Int -> Int -> Float -> IO Matrix
generateRandom rn cn ratio = do 
                              let obs_ratio = if ratio < 0 || ratio > 1 then 0.33 else ratio
                              let cant_obs = floor $ fromIntegral rn * fromIntegral cn * obs_ratio
                              obs <- sequence [genRCell rn cn (-1) | _ <-[1..cant_obs]]
                              let matrix = blankMatrix rn cn
                              let obs_matrix = foldl editMatrixCell matrix obs
                              first_cell <- genRCell rn cn 1
                              return (editMatrixCell obs_matrix first_cell)
                                    

generate :: Matrix
generate = read "{. x x x x x x x x \n . 8 x x x x x x x \n . . 11 x x x x x x \n 29 . 10 . x x x x x \n 30 . . . . x x x x \n . 31  1 38  .  . x x x \n . 32 . . 39 41 . x x \n . . . 22 . . 42 . x \n . . . . . . . 44 45}" :: Matrix

stepMatrix :: Int -> Matrix -> Cell -> Map Int Cell -> StdGen -> [(Matrix, Cell)]
stepMatrix step m@(Matrix rs cs ma) prevCell map gen = if Map.notMember step map
                        then [
                              (Matrix rs cs (Set.insert cell ma), cell) | cell <- getAdjacents prevCell rs cs step gen,
                              let actCell = Set.elemAt (Set.findIndex cell  ma) ma,
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

solveRecursiveDFS :: Matrix -> Int -> Cell -> Int -> Map Int Cell -> StdGen -> [Matrix]
solveRecursiveDFS actualMatrix step prevCell obs map gen
      | step == obs + 1 = [actualMatrix]
      | otherwise = let toAdd = stepMatrix step actualMatrix prevCell map gen
                    in concat [ solveRecursiveDFS matrix (step + 1) prevCell obs map gen | (matrix, prevCell) <- toAdd ]

solveAll :: Matrix -> StdGen -> [Matrix]
solveAll m = solveRecursiveDFS m 1 (Cell 0 0 0) (rows m * columns m  - countObstacles m) (buildMap m)

solve :: Matrix -> StdGen -> Matrix
solve m gen = let solves = solveAll m gen
          in case solves of [] -> error "Solve not found"
                            (x: xs) -> x
