module Algorithms
( generate
, solve
) where

import Structures

generate :: Matrix
generate = read "{9 . . \n x . x \n . . 1}" :: Matrix

solve :: Matrix -> Matrix
solve m = m
