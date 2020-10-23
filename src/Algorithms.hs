module Algorithms
( Dificulty(..)
, generate
, solve
, solveAll
, generateGame
, generateRandom
, validateTemplate
) where

import System.Random
import Structures
import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map
import System.Timeout
import Data.Maybe

genRCell :: Int -> Int -> Int -> IO Cell
genRCell rn cn val = do
      r <- randomRIO (1, rn)
      c <- randomRIO (1, cn)
      return $ Cell r c val

genRCellFromSet :: Set Cell -> IO [Cell]
genRCellFromSet set = if Set.null set
      then do
            return []
      else do
            let size = Set.size set
            r <- randomRIO (0, size - 1)
            let e = Set.elemAt r set
            let newSet = Set.deleteAt r set
            sets <- genRCellFromSet newSet
            return $ e : sets

generateRandom :: Int -> Int -> Float -> IO Matrix
generateRandom rn cn ratio = do
      let obs_ratio = if ratio < 0 || ratio > 1 then 0.33 else ratio
      let cant_obs = floor $ fromIntegral rn * fromIntegral cn * obs_ratio
      let (Matrix _ _ cells) = darkMatrix rn cn
      randomCells <- genRCellFromSet cells
      let matrix = blankMatrix rn cn
      let obs_matrix = foldl editMatrixCell matrix (take cant_obs randomCells)
      first_cell <- genRCell rn cn 1
      let newMatrix = editMatrixCell obs_matrix first_cell
      if validateTemplate newMatrix then do
            return newMatrix
      else do
            result <- generateRandom rn cn ratio
            return result

data Dificulty = Easy | Normal | Hard deriving (Ord, Eq, Show, Read)

emptyRatio :: Dificulty -> Float
emptyRatio Easy = 50/100
emptyRatio Normal = 60/100
emptyRatio Hard = 70/100

generateRandomGame :: Int -> Int -> Float -> IO Matrix
generateRandomGame rn cn ratio = do
      maybeTemplate <- timeout 1000000 $ generateRandom rn cn ratio
      if isNothing maybeTemplate then do
            game <- generateRandomGame rn cn ratio
            return game
      else do
            let template = maybe (blankMatrix rn cn) (\x -> x) maybeTemplate
            seed <- randomIO :: IO Int
            let gen = mkStdGen seed
            let seeds = randoms gen :: [Int]
            let solutions = solveAll template seeds
            if null solutions then do
                  game <- generateRandomGame rn cn ratio
                  return game
            else do
                  return $ head solutions

removeCells :: Matrix -> [Cell] -> Int -> Int -> [Int] -> Matrix
removeCells sol@(Matrix rn cn cs) cells ite n seeds =
      if    null cells || ite >= n then
            sol
      else
            let   (headCell: tailCells) = cells
                  rowCell = row headCell
                  colCell = column headCell
                  empty = Cell rowCell colCell 0
                  matrix = editMatrixCell sol empty
                  solutions = solveAll matrix seeds
                  isUnix = length (take 2 solutions) < 2
            in    if isUnix then
                        removeCells matrix tailCells (ite + 1) n seeds
                  else
                        removeCells sol tailCells (ite + 1) n seeds

generateGame :: Int -> Int -> Float -> Dificulty -> IO Matrix
generateGame rn cn ratio dif = do
      maybeMatrix <- timeout 60000000 $ generateRandomGame rn cn ratio
      if isNothing maybeMatrix then do
            return $ error "Game not found"
      else do
            let solution = maybe (blankMatrix rn cn) (\x -> x) maybeMatrix
            let setForRemove = Set.filter (\x -> let v = value x in v > 1 && v < rn * cn) (matrix solution) :: Set Cell
            randomCells <- genRCellFromSet setForRemove
            let total = Set.size setForRemove
            let cant_empty = floor $ fromIntegral total * emptyRatio dif
            seed <- randomIO :: IO Int
            let gen = mkStdGen seed
            let seeds = randoms gen :: [Int]
            let game = removeCells solution randomCells 0 cant_empty seeds
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

validateTemplate :: Matrix -> Bool
validateTemplate (Matrix rn cn cells)
      | sum [ 1 | degree <- degrees, degree == 0 ] > 0 = False 
      | sum [ 1 | degree <- degrees, degree == 1 ] > 2 = False
      | otherwise                                      = True
      where degrees = [ length adjacents | cell <- Set.toList cells, value cell >= 0, 
                        let adjacents = [ adjR | adj <- getAdjacents cell rn cn 0 [1..],
                                          let Just adjR = Set.lookupGE adj cells, adjR /= cell, value adjR == 0]]

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
