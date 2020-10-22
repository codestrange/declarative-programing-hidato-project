module Main where

import System.Random

import Structures
import Algorithms


main = do 
        let rand = randomIO :: IO Int
        seed <- rand
        let gen = mkStdGen seed
        let seeds = randoms gen :: [Int]

        print "Introduzca el MENOR tamaño posible del tablero:"
        minString <- getLine
        let minInput = read minString :: Int
        print "Introduzca el MAYOR tamaño posible del tablero:"
        maxString <- getLine
        let maxInput = read maxString :: Int
        print "Introduzca la RAZÓN de obstaculos del tablero:"
        ratioString <- getLine
        let ratioInput = read ratioString :: Float
        m <- generateRandom minInput maxInput ratioInput
        print m
        let sol = solve m seeds
        print sol
