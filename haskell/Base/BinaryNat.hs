module Base.BinaryNat (BinaryNat, zero, succ, add, mul, test) where

import qualified Prelude
import qualified Base.Boolean as Boolean
import qualified Base.Natural as Natural

data BinaryNat = Start | Digit BinaryNat Boolean.Boolean
    deriving (Prelude.Show)

zero :: BinaryNat
zero = Start

succ :: BinaryNat -> BinaryNat
succ Start = Digit Start Boolean.True
succ (Digit x Boolean.False) = Digit x Boolean.True
succ (Digit x Boolean.True) = Digit (succ x) Boolean.False

decr :: BinaryNat -> BinaryNat
decr Start = Prelude.undefined -- should not happen
decr (Digit Start Boolean.True) = Start
decr (Digit x Boolean.True) = Digit x Boolean.False
decr (Digit x Boolean.False) = Digit (decr x) Boolean.True

addCarry :: Boolean.Boolean -> BinaryNat -> BinaryNat -> BinaryNat
addCarry Boolean.False Start x = x
addCarry Boolean.False x Start = x
addCarry Boolean.True Start x = succ x
addCarry Boolean.True x Start = succ x
addCarry a (Digit x b) (Digit y c) = Digit (addCarry d x y) e where
    d = Boolean.maj a b c
    e = Boolean.xor (Boolean.xor a b) c

add :: BinaryNat -> BinaryNat -> BinaryNat
add = addCarry Boolean.False

mul :: BinaryNat -> BinaryNat -> BinaryNat
mul Start _ = Start
mul (Digit x Boolean.False) y = Digit (mul x y) Boolean.False
mul (Digit x Boolean.True) y = add y (Digit (mul x y) Boolean.False)

-- need proof that we never have Digit Start False

test :: BinaryNat -> a -> (BinaryNat -> a) -> a
test Start s _ = s
test x _ f = f (decr x)

instance Natural.Natural BinaryNat where
    zero = zero
    succ = succ
    add = add
    mul = mul
    test = test
