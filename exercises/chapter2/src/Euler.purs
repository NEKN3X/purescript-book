module Euler where

import Prelude

import Data.Foldable (sum)
import Data.List (List, filter, range)

ns ∷ Int → List Int
ns n = range 0 (n - 1)

multiples ∷ Int → List Int
multiples n = filter (\m -> mod m 3 == 0 || mod m 5 == 0) (ns n)

answer ∷ Int → Int
answer n = sum (multiples n)
