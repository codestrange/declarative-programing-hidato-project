module Algorithms
( generate
, solve
, solveAll
) where

import Structures


generate :: Matrix
generate = read "{7 . . \n x . x \n . . 1}" :: Matrix

stepMatrix :: Int -> Matrix -> Cell -> [(Matrix, Cell)]
stepMatrix step m prevCell = if null pCells 
                        then [
                              (editMatrixCell m actCell, actCell) | cell <- matrix m,
                              let actCell = Cell (row cell) (column cell) step,
                              isAdjacent cell prevCell, 
                              value cell == 0
                        ]
                        else  let (actCell : _) = pCells
                              in [(m, actCell) | isAdjacent actCell prevCell || value actCell == 1]
                        where pCells = findCellByValue m step

-- solveRecursiveBFS :: [(Matrix, Int)] -> [Matrix]
-- solveRecursiveBFS [] = []
-- solveRecursiveBFS ((actualMatrix, step):queue) 
--       | isFinalMatrix actualMatrix = actualMatrix:solveRecursiveBFS queue
--       | otherwise                  = let toAdd = [ (matrix, step + 1) 
--                                                       | matrix <- stepMatrix step actualMatrix]
--                                      in solveRecursiveBFS (queue ++ toAdd)

solveRecursiveDFS :: Matrix -> Int -> Cell -> [Matrix]
solveRecursiveDFS actualMatrix step prevCell
      | isFinalMatrix actualMatrix = [actualMatrix]
      | otherwise                  = let toAdd = stepMatrix step actualMatrix prevCell
                                     in concat [ solveRecursiveDFS matrix (step + 1) prevCell | (matrix, prevCell) <- toAdd ]

solveAll :: Matrix -> [Matrix]
solveAll m = solveRecursiveDFS m 1 (Cell 0 0 0)

solve :: Matrix -> Matrix
solve m = let solves = solveRecursiveDFS m 1 (Cell 0 0 0)
          in case solves of [] -> error "Solve not found"
                            (x: xs) -> x
