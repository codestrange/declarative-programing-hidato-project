module Algorithms
( Dificulty(..)
, generate
, solve
, solveAll
, generateGame
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

data Dificulty = Easy | Normal | Hard deriving (Ord, Eq, Show, Read)

emptyRatio :: Dificulty -> Float
emptyRatio Easy = 50/100
emptyRatio Normal = 60/100
emptyRatio Hard = 70/100

generateGame :: Int -> Int -> Float -> Dificulty -> IO Matrix
generateGame rn cn ratio dif = do
                                template <- generateRandom rn cn ratio
                                seed <- randomIO :: IO Int
                                let gen = mkStdGen seed
                                let seeds = randoms gen :: [Int]
                                let solution = solve template seeds
                                let cant_obs = countObstacles solution
                                let total = rn * cn - cant_obs
                                let cant_empty = floor $ fromIntegral total * emptyRatio dif
                                rawEmpty <- sequence [genRCell rn cn 0 | _ <- [1..cant_empty]]
                                let empty = filter (\cell -> let Just act = Set.lookupGE cell (matrix solution)
                                                            in value act > 1 && value act < total)
                                                rawEmpty
                                let game = foldl editMatrixCell solution empty
                                return game

generate :: Matrix
generate = read "{. x x x x x x x x \n . 8 x x x x x x x \n . . 11 x x x x x x \n 29 . 10 . x x x x x \n 30 . . . . x x x x \n . 31  1 38  .  . x x x \n . 32 . . 39 41 . x x \n . . . 22 . . 42 . x \n . . . . . . . 44 45}" :: Matrix

stepMatrix :: Int -> Matrix -> Cell -> Map Int Cell -> [Int] -> [(Matrix, Cell)]
stepMatrix step m@(Matrix rs cs ma) prevCell map seeds = if Map.notMember step map
                        then [
                              newMatrix | cell <- getAdjacents prevCell rs cs step seeds,
                              let actCell = Set.elemAt (Set.findIndex cell ma) ma,
                              value actCell == 0,
                              let newMatrix = (Matrix rs cs (Set.insert cell ma), cell)
                        ]
                        else  let actCell = map Map.! step
                              in [newMatrix | isAdjacent actCell prevCell || value actCell == 1, let newMatrix = (m, actCell)]

buildMap :: Matrix -> Map Int Cell
buildMap ma@(Matrix r c m) = Set.foldl (\acc cell -> Map.insert (value cell) cell acc) Map.empty m

-- solveRecursiveBFS :: [(Matrix, Int)] -> [Matrix]
-- solveRecursiveBFS [] = []
-- solveRecursiveBFS ((actualMatrix, step):queue) 
--       | isFinalMatrix actualMatrix = actualMatrix:solveRecursiveBFS queue
--       | otherwise                  = let toAdd = [ (matrix, step + 1) 
--                                                       | matrix <- stepMatrix step actualMatrix]
--                                      in solveRecursiveBFS (queue ++ toAdd)

solveRecursiveDFS :: Matrix -> Int -> Cell -> Int -> Map Int Cell -> [Int] -> [Matrix]
solveRecursiveDFS actualMatrix step prevCell obs map seeds
      | step == obs + 1 = [actualMatrix]
      | otherwise = let toAdd = stepMatrix step actualMatrix prevCell map seeds
                    in concat [ solveRecursiveDFS matrix (step + 1) prevCell obs map (tail seeds) | (matrix, prevCell) <- toAdd ]

solveAll :: Matrix -> [Int] -> [Matrix]
solveAll m = solveRecursiveDFS m 1 (Cell 0 0 0) (rows m * columns m  - countObstacles m) (buildMap m)

solve :: Matrix -> [Int] -> Matrix
solve m seeds = let solves = solveAll m seeds
          in case solves of [] -> error "Solve not found"
                            (x: xs) -> x
