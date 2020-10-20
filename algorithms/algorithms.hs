module Algorithms
( generate
, solve
) where

import Structures


generate :: Matrix
generate = read "{7 . . \n x . x \n . . 1}" :: Matrix

stepMatrix :: Int -> Matrix -> [Matrix]
stepMatrix step m = if null pCells 
                        then [ editMatrixCell m (Cell (row cell) (column cell) step)
                                 | cell <- matrix m, isAdjacent cell prevCell, value cell == 0 ]
                        else let (actCell:_)     = pCells
                             in [ m | isAdjacent actCell prevCell || value actCell == 1]
                       where pCells    = findCellByValue m step
                             prevCells = findCellByValue m (step-1)
                             prevCell  = if null prevCells then Cell 0 0 0  else head prevCells 

solveRecursiveBFS :: [(Matrix, Int)] -> [Matrix]
solveRecursiveBFS [] = []
solveRecursiveBFS ((actualMatrix, step):queue) 
      | isFinalMatrix actualMatrix = actualMatrix:solveRecursiveBFS queue
      | otherwise                  = let toAdd = [ (matrix, step + 1) 
                                                      | matrix <- stepMatrix step actualMatrix]
                                     in solveRecursiveBFS (queue ++ toAdd)

solve :: Matrix -> Matrix
solve m = m
