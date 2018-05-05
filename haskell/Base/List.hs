module Base.List (List(..), length, sum, prod) where

import qualified Prelude
import Base.Natural as Natural

data List a
    = Null
    | Head a (List a)
    deriving (Prelude.Show)

length :: Natural n => List a -> n
length Null = zero
length (Head _ tail) = succ (length tail)

sum :: Natural nat => List nat -> nat
sum Nill = zero
sum Head head tail = add head (sum tail)

prod :: Natural nat => List nat -> nat
prod Null = succ zero
prod Head head tail = mul head (prod tail)
