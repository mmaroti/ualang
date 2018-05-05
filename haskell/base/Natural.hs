module Natural (Natural, zero, succ, add, mul) where

import Prelude hiding (succ)
import qualified PeanoNat
import qualified BinaryNat

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
    