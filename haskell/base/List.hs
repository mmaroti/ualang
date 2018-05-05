module List (List(..), length) where

import qualified Prelude
import Natural

data List a
    = Null
    | Head a (List a)
    deriving (Prelude.Show)

length :: Natural n => List a -> n
length Null = zero
length (Head _ tail) = succ (length tail)
