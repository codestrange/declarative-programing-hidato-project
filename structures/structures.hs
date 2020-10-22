module Structures
( Cell (..)
, Matrix (..)
, countInMatrix
, countFree
, countObstacles
, isAdjacent
, isValidMatrix
, isFinalMatrix
, editMatrixCell
, findCellByValue
, getAdjacents
) where

import Data.Char (isDigit)
import Data.List
import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set

data Cell = Cell { row :: Int
                , column :: Int
                , value :: Int
                }

instance Eq Cell where
    c1 == c2 = row c1 == row c2 && column c1 == column c2

instance Ord Cell where
    compare cell1 cell2
        | row cell1 /= row cell2 = compare (row cell1) (row cell2)
        | column cell1 /= column cell2 = compare (column cell1) (column cell2)
        | otherwise = EQ

instance Show Cell where
    show cell = "(" ++ show (row cell) ++ "," ++ show (column cell) ++ "," ++ show (value cell) ++ ")"

instance Read Cell where
    readsPrec _ input =
        let (opar : rest1)   = input
            (rows, rest2)    = span isDigit rest1
            (comma1:rest3)   = rest2
            (columns, rest4) = span isDigit rest3
            (comma2 : rest5) = rest4
            (values, rest6)  = span isDigit rest5
            (cpar : rest7)   = rest6
            row              = read rows :: Int
            column           = read columns :: Int   
            value            = read values :: Int
        in 
            [(Cell row column value, rest7) | 
                opar == '(' && comma1 == comma2 && comma2 == ',' && cpar == ')']

getAdjacents :: Cell -> Int -> Int -> Int -> [Cell]
getAdjacents (Cell r c v) rs cs s = [Cell nr nc s | dr <- [-1, 0, 1], dc <- [-1, 0, 1], let (nr, nc) = (r + dr, c + dc), nr > 0, nr <= rs, nc > 0, nc <= cs]

isAdjacent :: Cell -> Cell -> Bool
isAdjacent (Cell f1 c1 v1) (Cell f2 c2 v2)
            | Cell f1 c1 v1 == Cell f2 c2 v2           = False
            | abs (f1 - f2) >= 2 || abs (c1 - c2) >= 2 = False
            | v1 == (-1) || v2 == (-1)                 = False
            | otherwise                                = True

getCellChar :: Cell -> Int -> String
getCellChar (Cell _ _ value) size
    | value <  0    = (++) "x" $ concat $ replicate (max (size - 1) 0) " "
    | value == 0    = (++) "." $ concat $ replicate (max (size - 1) 0) " "
    | otherwise     = (++) valueStr $ concat $ replicate (max (size - lenValueStr) 0) " " 
    where
        valueStr    = show value
        lenValueStr = length $ show value

cellEqual :: Cell -> Cell -> Bool
cellEqual x y = row x == row y && column x == column y && value x == value y

cellEquals' :: [Cell] -> [Cell] -> Bool -> Bool
callEquals' _ _ False = False
cellEquals' [] [] x = x && True
cellEquals' _ [] _ = False
cellEquals' [] _ _ = False
cellEquals' (x:xs) (y:ys) b = cellEquals' xs ys (cellEqual x y)

cellEquals :: [Cell] -> [Cell] -> Bool
cellEquals xs ys = cellEquals' xs ys True

data Matrix = Matrix { rows :: Int
                     , columns :: Int
                     , matrix :: Set Cell
                     }

instance Eq Matrix where
    m1 == m2 = rows m1 == rows m2 && columns m1 == columns m2 && cellEquals (Set.elems $ matrix m1) (Set.elems $ matrix m2)

showMatrixRow :: [Cell] -> Int -> String
showMatrixRow rowCells size = unwords [getCellChar cell size | cell <- rowCells ]

instance Show Matrix where
    show m = "{" ++ intercalate "\n " [ showMatrixRow (sort (filter (\(Cell crow _ _) -> crow == row) (Set.elems $ matrix m))) maxSize |
                            row <- [1..(rows m)]] ++ "}\n" where
                            maxSize = maximum [length (getCellChar cell 0) | cell <- Set.elems $ matrix m]

parseMatrixCell :: String -> (Int, String)
parseMatrixCell "" = (-2, "")
parseMatrixCell (s:rest)
    | isDigit s = let (nums,rest1) = span isDigit (s:rest) in
                    (read nums :: Int, rest1)
    | s == '.'  = (0, rest)
    | s == 'x'  = (-1, rest)
    | otherwise = (-2, s:rest)

parseMatrixRowRecursive :: Int -> Int -> String -> (Set Cell, String)
parseMatrixRowRecursive rowNum columnNum input = 
    let (_, rinput)    = span (' '==) input
        (value, rest1) = parseMatrixCell rinput
    in if value == -2 then (Set.empty, rest1) else
        let (parsedCells, frest) = parseMatrixRowRecursive rowNum (columnNum + 1) rest1
            parsedCell           = Cell rowNum columnNum value
        in (Set.insert parsedCell parsedCells, frest)

parseMatrixRow rowNum = parseMatrixRowRecursive rowNum 1

parseMatrixRecursive :: Int -> String -> (Set Cell, String)
parseMatrixRecursive rowNum (s:rest)
    | (s == '{' && rowNum == 1) || s == '\n' = let (parsedRow, rest1)  = parseMatrixRow rowNum rest
                                                   (parsedRows, rest2) = parseMatrixRecursive (rowNum + 1) rest1
                                                in (Set.union parsedRow parsedRows, rest2)
    | s == '}'                               = (Set.empty, rest)

parseMatrix = parseMatrixRecursive 1

countInMatrix :: Int -> Matrix -> Int
countInMatrix val (Matrix _ _ cells) =
                    foldl (\acc cell -> if value cell == val then acc + 1 else acc) 0 cells

countObstacles :: Matrix -> Int
countObstacles = countInMatrix (-1) 

countFree :: Matrix -> Int
countFree = countInMatrix 0 

isValidMatrix :: Matrix -> Bool
isValidMatrix m = let allCells       = length (matrix m) == rows m * columns m
                      correctValues  = all (\(Cell _ _ val) -> val >= -1 && val <= rows m * columns m - countObstacles m) 
                                        (matrix m)                    
                  in correctValues && allCells

isFinalMatrix :: Matrix -> Int -> Int -> Bool
isFinalMatrix m@(Matrix rows columns cells) step obs
        | step < notObs = False
        | or [ value cell == 0 | cell <- Set.elems cells ] = False
        | not (and [ countInMatrix val m == 1 | val <- [1..notObs] ]) = False
        | not (and [ or [ value cell1 + 1 == value cell2 | cell2 <- Set.elems cells, isAdjacent cell1 cell2 ]
                                     | cell1 <- Set.elems cells,
                                      value cell1 /= fvalue,
                                      value cell1 /= (-1) ]) = False
        | otherwise = True
        where fvalue = maximum [value cell | cell <- Set.elems cells]
              notObs = rows * columns - obs

instance Read Matrix where
    readsPrec _ input =
        let (matrixCells, rest) = parseMatrix input
            rowNum              = maximum [ row cell | cell <- Set.elems matrixCells ]
            columnNum           = maximum [ column cell | cell <- Set.elems matrixCells ]
            theMatrix           = Matrix rowNum columnNum matrixCells
        in [(theMatrix, rest) | isValidMatrix theMatrix]

editMatrixCell :: Matrix -> Cell -> Matrix
editMatrixCell (Matrix r c cells) newCell = Matrix r c (Set.fromList ([ if cell == newCell then newCell else cell | cell <- Set.elems cells ]) :: Set Cell )

findCellByValue :: Matrix -> Int -> Set Cell
findCellByValue m val = Set.filter (\cell -> value cell == val) $ matrix m
