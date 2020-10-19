module Structures
( Cell (..)
, Matrix (..)
) where

import System.IO
import Data.Char (isDigit)
import Data.List


data Cell = Cell { row :: Int
                , column :: Int
                , value :: Int
                } deriving (Eq)

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

validMatrix :: Matrix -> Bool
validMatrix m = let allCells      = length (matrix m) == rows m * columns m
                    correctValues = all (\(Cell _ _ val) -> val >= -1 && val <= rows m * columns m) 
                                        (matrix m)
                    in correctValues && allCells

instance Read Matrix where
    readsPrec _ input =
        let (matrixCells, rest) = parseMatrix input
            rowNum              = maximum [ row cell | cell <- matrixCells ]
            columnNum           = maximum [ column cell | cell <- matrixCells ]
            theMatrix           = Matrix rowNum columnNum matrixCells
        in [(theMatrix, rest) | validMatrix theMatrix]
