module Test.MySolutions where

import Prelude

import Data.Array (nub, nubEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (class Newtype, over2, overF, wrap)

-- Note to reader: Add your solutions to this file

newtype Point = Point { x :: Number, y :: Number }

instance Show Point where
  show (Point p) = "(" <> show p.x <> ", " <> show p.y <> ")"

newtype Complex = Complex { real :: Number, imaginary :: Number }

instance Show Complex where
  show (Complex c) = "" <> show c.real <> (if c.imaginary >= 0.0 then "+" else "") <> show c.imaginary <> "i"

derive instance Eq Complex

derive instance Newtype Complex _

instance Semiring Complex where
  add = over2 Complex add
  mul = over2 Complex
    \{ real: r1, imaginary: i1 }
     { real: r2, imaginary: i2 } ->
      { real: r1 * r2 - i1 * i2
      , imaginary: r1 * i2 + r2 * i1
      }
  zero = wrap zero
  one = wrap { real: one, imaginary: zero }

derive newtype instance Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance Generic Shape _

instance Show Shape where
  show (Circle p r) = "(Circle " <> show p <> " " <> show r <> ")"
  show (Rectangle p w h) = "(Rectangle " <> show p <> " " <> show w <> " " <> show h <> ")"
  show (Line p1 p2) = "(Line " <> show p1 <> " " <> show p2 <> ")"
  show (Text p s) = "(Text " <> show p <> " \"" <> s <> "\")"

data NonEmpty a = NonEmpty a (Array a)

instance Show a => Show (NonEmpty a) where
  show (NonEmpty e1 a1) = show e1 <> " " <> show a1

instance Eq a => Eq (NonEmpty a) where
  eq (NonEmpty e1 a1) (NonEmpty e2 a2) = e1 == e2 && a1 == a2

instance Semigroup (NonEmpty a) where
  append (NonEmpty e1 a1) (NonEmpty e2 a2) = NonEmpty e1 (a1 <> [ e2 ] <> a2)

derive instance Functor NonEmpty

data Extended a = Infinite | Finite a

derive instance Eq a => Eq (Extended a)

instance Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite a) (Finite b) = compare a b

instance Foldable NonEmpty where
  foldr f z (NonEmpty e a) = foldr f z ([ e ] <> a)
  foldl f z (NonEmpty e a) = foldl f z ([ e ] <> a)
  foldMap f (NonEmpty e a) = foldMap f ([ e ] <> a)

data OneMore f a = OneMore a (f a)

instance Foldable f => Foldable (OneMore f) where
  foldr func st (OneMore val more) = func val lastB
    where
    lastB = foldr func st more
  foldl func st (OneMore val more) = foldl func firstB more
    where
    firstB = (func st val)
  foldMap func (OneMore val more) = (func val) <> (foldMap func more)

derive instance Eq Point
derive instance Eq Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes shapes = nubEq shapes

derive instance Ord Point
derive instance Ord Shape

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast shapes = nub shapes

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum arr = case maximum arr of
  Just m -> m

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance Monoid Multiply where
  mempty = Multiply 1

instance Action Multiply Int where
  act (Multiply n) a = a * n

instance Action Multiply String where
  act (Multiply n) s = power s n

derive newtype instance Show Multiply
derive newtype instance Eq Multiply

instance Action m a => Action m (Array a) where
  act m arr = map (act m) arr

newtype Self m = Self m

instance Monoid m => Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)

derive newtype instance Show m => Show (Self m)
derive newtype instance Eq m => Eq (Self m)
