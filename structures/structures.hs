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
) where

import Data.Char (isDigit)
import Data.List

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
        let (opar:rest1)     = input
            (rows, rest2)    = span isDigit rest1
            (comma1:rest3)   = rest2
            (columns, rest4) = span isDigit rest3
            (comma2:rest5)   = rest4
            (values, rest6)  = span isDigit rest5
            (cpar:rest7)         = rest6
            row              = read rows :: Int
            column           = read columns :: Int   
            value            = read values :: Int
        in 
            [(Cell row column value, rest7) | 
                opar == '(' && comma1 == comma2 && comma2 == ',' && cpar == ')']

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

data Matrix = Matrix { rows :: Int
                     , columns :: Int
                     , matrix :: [Cell]
                     }

instance Eq Matrix where
    m1 == m2 = rows m1 == rows m2 && columns m1 == columns m2 && sort (matrix m1) == sort (matrix m2)

showMatrixRow :: [Cell] -> Int -> String
showMatrixRow rowCells size = unwords [getCellChar cell size | cell <- rowCells ]

instance Show Matrix where
    show m = "{" ++ intercalate "\n " [ showMatrixRow (sort (filter (\(Cell crow _ _) -> crow == row) (matrix m))) maxSize |
                            row <- [1..(rows m)]] ++ "}" where
                            maxSize = maximum [length (getCellChar cell 0) | cell <- matrix m]

parseMatrixCell :: String -> (Int, String)
parseMatrixCell "" = (-2, "")
parseMatrixCell (s:rest)
    | isDigit s = let (nums,rest1) = span isDigit (s:rest) in
                    (read nums :: Int, rest1)
    | s == '.'  = (0, rest)
    | s == 'x'  = (-1, rest)
    | otherwise = (-2, s:rest)

parseMatrixRowRecursive :: Int -> Int -> String -> ([Cell], String)
parseMatrixRowRecursive rowNum columnNum input = 
    let (_, rinput)    = span (' '==) input
        (value, rest1) = parseMatrixCell rinput
    in if value == -2 then ([], rest1) else
        let (parsedCells, frest) = parseMatrixRowRecursive rowNum (columnNum+1) rest1
            parsedCell           = Cell rowNum columnNum value
        in (parsedCell:parsedCells, frest)

parseMatrixRow rowNum = parseMatrixRowRecursive rowNum 1

parseMatrixRecursive :: Int -> String -> ([Cell], String)
parseMatrixRecursive rowNum (s:rest)
    | (s == '{' && rowNum == 1) || s == '\n' = let (parsedRow, rest1)  = parseMatrixRow rowNum rest
                                                   (parsedRows, rest2) = parseMatrixRecursive (rowNum+1) rest1
                                                in (parsedRow ++ parsedRows, rest2)
    | s == '}'                               = ([], rest)

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

isFinalMatrix :: Matrix -> Bool
isFinalMatrix (Matrix rows columns cells)
        | or [ value cell == 0 | cell <- cells ]             = False
        | not (and [ countInMatrix val (Matrix rows columns cells) == 1
                                     | val <- [1..(rows * columns - countObstacles (Matrix rows columns cells))]
                                      ])                     = False
        | not (and [ or [ value cell1 + 1 == value cell2 | cell2 <- cells, isAdjacent cell1 cell2 ]
                                     | cell1 <- cells,
                                      value cell1 /= fvalue,
                                      value cell1 /= (-1) ]) = False
        | otherwise                                          = True
        where fvalue = maximum [value cell | cell <- cells]

instance Read Matrix where
    readsPrec _ input =
        let (matrixCells, rest) = parseMatrix input
            rowNum              = maximum [ row cell | cell <- matrixCells ]
            columnNum           = maximum [ column cell | cell <- matrixCells ]
            theMatrix           = Matrix rowNum columnNum matrixCells
        in [(theMatrix, rest) | isValidMatrix theMatrix]

editMatrixCell :: Matrix -> Cell -> Matrix
editMatrixCell (Matrix r c cells) newCell = Matrix r c [ if cell == newCell then newCell else cell  | cell <- cells ] 

findCellByValue :: Matrix -> Int -> [Cell]
findCellByValue m val =  [ cell | cell <- matrix m, value cell == val ]
