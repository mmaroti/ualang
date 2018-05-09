module Base.List (List(..), length, sum, product, map) where

import qualified Prelude
import qualified Base.Natural as Natural

data List a
    = Null
    | Cons a (List a)
    deriving (Prelude.Show)

length :: Natural.Natural n => List a -> n
length Null = Natural.zero
length (Cons _ tail) = Natural.succ (length tail)

sum :: Natural.Natural nat => List nat -> nat
sum Null = Natural.zero
sum (Cons head tail) = Natural.add head (sum tail)

product :: Natural.Natural nat => List nat -> nat
product Null = Natural.succ Natural.zero
product (Cons head tail) = Natural.mul head (product tail)

map :: (a -> b) -> List a -> List b
map _ Null = Null
map fun (Cons head tail) = Cons (fun head) (map fun tail)

-- replicate :: Natural nat => nat -> a > List add
-- replicate 
