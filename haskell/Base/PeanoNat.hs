module Base.PeanoNat (PeanoNat, zero, succ, add, mul, test) where

import qualified Prelude
import qualified Base.Natural as Natural

data PeanoNat = Zero | Succ PeanoNat
    deriving (Prelude.Show)

zero :: PeanoNat
zero = Zero

succ :: PeanoNat -> PeanoNat
succ = Succ 

add :: PeanoNat -> PeanoNat -> PeanoNat
add Zero y = y
add (Succ x) y = add x (Succ y)

mul :: PeanoNat -> PeanoNat -> PeanoNat
mul Zero _ = Zero
mul (Succ x) y = add y (mul x y)

test :: PeanoNat -> a -> (PeanoNat -> a) -> a
test Zero s _ = s
test (Succ x) _ f = f x

instance Natural.Natural PeanoNat where
    zero = zero
    succ = succ
    add = add
    mul = mul
    test = test
