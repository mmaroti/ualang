module Base.Natural (Natural, zero, succ, add, mul) where

import qualified Prelude
import qualified Base.PeanoNat as PeanoNat
import qualified Base.BinaryNat as BinaryNat

class Natural nat where
    zero :: nat
    succ :: nat -> nat
    add :: nat -> nat -> nat
    mul :: nat -> nat -> nat

instance Natural PeanoNat.PeanoNat where
    zero = PeanoNat.zero
    succ = PeanoNat.succ
    add = PeanoNat.add
    mul = PeanoNat.mul

instance Natural BinaryNat.BinaryNat where
    zero = BinaryNat.zero
    succ = BinaryNat.succ
    add = BinaryNat.add
    mul = BinaryNat.mul

instance Natural Prelude.Integer where
    zero = 0
    succ x = x Prelude.+ 1
    add x y = x Prelude.+ y
    mul x y = x Prelude.* y
