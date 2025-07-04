module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (concat, filter, foldl, head, length, tail, (..), (:))
import Data.Maybe (Maybe, fromMaybe)
import Data.Path (Path, filename, isDirectory, ls, size)
import Test.Examples (allFiles', factors)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven n =
  if n < 0 then isEven (-n)
  else if n == 0 then true
  else
    not isEven (n - 1)

oneIfEven :: Int -> Int
oneIfEven n = if isEven n then 1 else 0

countEven :: Array Int -> Int
countEven [] = 0
countEven xs = oneIfEven (fromMaybe 1 $ head xs) + countEven (fromMaybe [] $ tail xs)

squared :: Array Number -> Array Number
squared [] = []
squared xs = (\x -> x * x) <$> xs

keepNonNegative :: Array Number -> Array Number
keepNonNegative xs = filter (\x -> x >= 0.0) xs

infix 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite xs = (\x -> x >= 0.0) <$?> xs

isPrime :: Int -> Boolean
isPrime n = n > 1 && length (factors n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [ x, y ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- 1 .. n
  guard $ a * a + b * b == c * c
  pure [ a, b, c ]

primeFactors :: Int -> Array Int
primeFactors 1 = []
primeFactors n = concat [ as, primeFactors (n `div` multiple as) ]
  where
  as = do
    fs <- factors n
    x <- fs
    guard $ isPrime x
    pure x

  multiple :: Array Int -> Int
  multiple [] = 1
  multiple xs = fromMaybe 1 (head xs) * multiple (fromMaybe [] (tail xs))

allTrue :: Array Boolean -> Boolean
allTrue xs = foldl (&&) true xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibTailRec :: Int -> Int
fibTailRec n = fibTailRec' n 0
  where
  fibTailRec' :: Int -> Int -> Int
  fibTailRec' 0 acc = acc
  fibTailRec' 1 acc = acc + 1
  fibTailRec' n' acc = fibTailRec' (n' - 1) acc + fibTailRec' (n' - 2) acc

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [ x ] <> xs) []

onlyFiles :: Path -> Array Path
onlyFiles path = filter (not isDirectory) (allFiles' path)

whereIs :: Path -> String -> Maybe Path
whereIs path name = head files
  where
  files = do
    dir <- ls path
    file <- allFiles' path
    guard $ filename dir <> name == filename file
    pure dir

largestSmallest :: Path -> Array Path
largestSmallest path = foldl loop [] (onlyFiles path)
  where
  loop :: Array Path -> Path -> Array Path
  loop [ largest, smallest ] current
    | size current < size smallest = [ largest, current ]
    | size current > size largest = [ current, smallest ]
    | otherwise = [ largest, smallest ]
  loop [ last ] current
    | size current < size last = [ last, current ]
    | otherwise = [ current, last ]
  loop arr current = [ current ] <> arr
