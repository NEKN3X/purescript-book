module Test.MySolutions where

import Prelude

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial n k | k > n = 0
binomial n k = factorial n `div` (factorial k * factorial (n - k))

pascal :: Int -> Int -> Int
pascal n k = binomial (n - 1) k + binomial (n - 1) (k - 1)
