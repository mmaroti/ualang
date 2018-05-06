module Base.List (List(..), length, sum, product, map) where

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
sum Null = zero
sum (Head head tail) = add head (sum tail)

product :: Natural nat => List nat -> nat
product Null = succ zero
product (Head head tail) = mul head (product tail)

map :: (a -> b) -> List a -> List b
map _ Null = Null
map fun (Head head tail) = Head (fun head) (map fun tail)

-- replicate :: Natural nat => nat -> a > List add
-- replicate 
