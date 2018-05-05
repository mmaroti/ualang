module BinaryNat (BinaryNat, zero, succ, add, mul) where

import qualified Prelude
import Boolean

data BinaryNat = Falses | Digit BinaryNat Boolean
    deriving (Prelude.Show)

zero :: BinaryNat
zero = Falses

succ :: BinaryNat -> BinaryNat
succ Falses = Digit Falses True
succ (Digit x False) = Digit x True
succ (Digit x True) = Digit (succ x) False

addCarry :: Boolean -> BinaryNat -> BinaryNat -> BinaryNat
addCarry False Falses x = x
addCarry False x Falses = x
addCarry True Falses x = succ x
addCarry True x Falses = succ x
addCarry a (Digit x b) (Digit y c) = Digit (addCarry d x y) e where
    d = maj a b c
    e = xor (xor a b) c

add :: BinaryNat -> BinaryNat -> BinaryNat
add = addCarry False

mul :: BinaryNat -> BinaryNat -> BinaryNat
mul Falses _ = Falses
mul (Digit x False) y = Digit (mul x y) False
mul (Digit x True) y = add y (Digit (mul x y) False)
