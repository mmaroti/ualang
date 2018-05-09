module Base.Natural (Natural, zero, succ, add, mul, test) where

import qualified Prelude

class Natural nat where
    zero :: nat
    succ :: nat -> nat
    add :: nat -> nat -> nat
    mul :: nat -> nat -> nat
    test :: nat -> a -> (nat -> a) -> a

instance Natural Prelude.Integer where
    zero = 0
    succ x = x Prelude.+ 1
    add x y = x Prelude.+ y
    mul x y = x Prelude.* y
    test x s f = if x Prelude.<= 0 
        then s 
        else f (x Prelude.- 1)
