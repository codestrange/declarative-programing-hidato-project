module Main where

import System.Random

import Structures
import Algorithms


main = do 
        let rand = randomIO :: IO Int
        seed <- rand
        let gen = mkStdGen seed

        let m = generate
        let sol = solve m gen
        print sol
