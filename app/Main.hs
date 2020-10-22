module Main where

import System.Random

import Structures
import Algorithms


main = do 
        let rand = randomIO :: IO Int
        seed <- rand
        let gen = mkStdGen seed
        let seeds = randoms gen :: [Int]

        print "Introduzca la cantidad de filas:"
        rowsString <- getLine
        let rows = read rowsString :: Int
        print "Introduzca la cantidad de columnas:"
        columnsString <- getLine
        let columns = read columnsString :: Int
        print "Introduzca el radio de obstaculos:"
        ratioString <- getLine
        let ratio = read ratioString :: Float
        print "Introduzca la dificultad:"
        dificultyString <- getLine
        let dificulty = read dificultyString :: Dificulty
        m <- generateGame rows columns ratio dificulty
        print m
        let sol = solve m seeds
        print sol
