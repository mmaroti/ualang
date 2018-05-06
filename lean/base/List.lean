
import .PeanoNat
--.Natural, 

inductive List (a : Type)
| Null {} : List
| Head (h : a) (t : List) : List

open PeanoNat

def length (a : Type) : List a -> PeanoNat
| Null := zero
| Head _ tail := succ (length tail)


def sum : List PeanoNat -> PeanoNat
| Null := zero
| Head head tail := add head (sum tail)

def product : List PeanoNat -> PeanoNat
| Null := succ zero
| Head head tail := mul head (product tail)

/-
variables a b:Type

def map : (a -> b) -> List a -> List b
| _ Null := Null
| map (Head head tail) := Head ( head) (map fun tail)
-/