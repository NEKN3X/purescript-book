module Data.Picture where

import Prelude
import Data.Foldable (foldl)
import Data.Number (infinity)
import Data.Number as Number

type Point =
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint { x, y } =
  "(" <> show x <> ", " <> show y <> ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) =
  "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"

exampleLine :: Shape
exampleLine = Line p1 p2
  where
  p1 :: Point
  p1 = { x: 0.0, y: 0.0 }

  p2 :: Point
  p2 = { x: 100.0, y: 50.0 }

origin :: Point
origin = { x, y }
  where
  x = 0.0
  y = 0.0

-- Would generally write it like this instead:
-- origin = { x: 0.0, y: 0.0 }

getCenter :: Shape -> Point
getCenter (Circle c r) = c
getCenter (Rectangle c w h) = c
getCenter (Line s e) = (s + e) * { x: 0.5, y: 0.5 }
getCenter (Text loc text) = loc

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

type Bounds =
  { top :: Number
  , left :: Number
  , bottom :: Number
  , right :: Number
  }

showBounds :: Bounds -> String
showBounds b =
  "Bounds [top: " <> show b.top
    <> ", left: "
    <> show b.left
    <> ", bottom: "
    <> show b.bottom
    <> ", right: "
    <> show b.right
    <>
      "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle { x, y } r) =
  { top: y - r
  , left: x - r
  , bottom: y + r
  , right: x + r
  }
shapeBounds (Rectangle { x, y } w h) =
  { top: y - h / 2.0
  , left: x - w / 2.0
  , bottom: y + h / 2.0
  , right: x + w / 2.0
  }
shapeBounds (Line p1 p2) =
  { top: Number.min p1.y p2.y
  , left: Number.min p1.x p2.x
  , bottom: Number.max p1.y p2.y
  , right: Number.max p1.x p2.x
  }
shapeBounds (Text { x, y } _) =
  { top: y
  , left: x
  , bottom: y
  , right: x
  }

union :: Bounds -> Bounds -> Bounds
union b1 b2 =
  { top: Number.min b1.top b2.top
  , left: Number.min b1.left b2.left
  , bottom: Number.max b1.bottom b2.bottom
  , right: Number.max b1.right b2.right
  }

intersect :: Bounds -> Bounds -> Bounds
intersect b1 b2 =
  { top: Number.max b1.top b2.top
  , left: Number.max b1.left b2.left
  , bottom: Number.min b1.bottom b2.bottom
  , right: Number.min b1.right b2.right
  }

emptyBounds :: Bounds
emptyBounds =
  { top: infinity
  , left: infinity
  , bottom: -infinity
  , right: -infinity
  }

infiniteBounds :: Bounds
infiniteBounds =
  { top: -infinity
  , left: -infinity
  , bottom: infinity
  , right: infinity
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = union (shapeBounds shape) b

{-
These `instance`s are to enable testing.
Feel free to ignore these.
They'll make more sense in the next chapter.
-}
derive instance Eq Shape

instance Show Shape where
  show shape = showShape shape
