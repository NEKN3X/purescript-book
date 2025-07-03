module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Person, Volt(..))
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Number (pi)
import Data.Number as Number
import Data.Picture (Bounds, Picture, Shape(..), Point, origin)
import Data.Picture as Picture
import Data.Picture as Shape
import Prim.TypeError (class Warn)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial n k | k > n = 0
binomial n k = factorial n `div` (factorial k * factorial (n - k))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal n k = binomial (n - 1) k + binomial (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } }
  | c1 == c2 = true
  | otherwise = false

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [ n ] = n
fromSingleton n _ = n

circleAtOrigin :: Shape
circleAtOrigin = Shape.Circle origin 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Shape.Circle _ r) =
  Shape.Circle origin (2.0 * r)
doubleScaleAndCenter (Shape.Rectangle _ w h) =
  Shape.Rectangle origin (2.0 * w) (2.0 * h)
doubleScaleAndCenter (Shape.Line p1 p2) =
  Shape.Line ({ x: 2.0 * (p1.x - center.x), y: 2.0 * (p1.y - center.y) }) ({ x: 2.0 * (p2.x - center.x), y: 2.0 * (p2.y - center.y) })
  where
  center = { x: (p1.x + p2.x) / 2.0, y: (p1.y + p2.y) / 2.0 }
doubleScaleAndCenter (Shape.Text _ s) = Shape.Text origin s

shapeText :: Shape -> Maybe String
shapeText (Shape.Text _ s) = Maybe.Just s
shapeText _ = Maybe.Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)

area :: Shape -> Number
area (Shape.Line _ _) = 0.0
area (Shape.Text _ _) = 0.0
area (Shape.Circle _ r) = pi * r * r
area (Shape.Rectangle _ w h) = w * h

data MyShape = Shape Shape | Clipped Picture Point Number Number

shapeBounds :: MyShape -> Bounds
shapeBounds (Shape s) = Picture.shapeBounds s
shapeBounds (Clipped pic _ _ _) = Picture.bounds pic
