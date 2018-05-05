module Base.PeanoNat (PeanoNat, zero, succ, add, mul) where

import qualified Prelude

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
