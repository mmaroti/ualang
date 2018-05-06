
import .Boolean

inductive BinaryNat
| Falses : BinaryNat
| Digit (n : BinaryNat) (b : Boolean) : BinaryNat

namespace BinaryNat
open Boolean

def zero : BinaryNat := Falses

def succ : BinaryNat → BinaryNat
| Falses := Digit Falses True
| (Digit x False) := Digit x True
| (Digit x True) := Digit (succ x) False

def addCarry : Boolean → BinaryNat → BinaryNat → BinaryNat
| False x Falses := x
| False Falses x := x
| True x Falses := succ x
| True Falses x := succ x
| a (Digit x b) (Digit y c) := 
    let d := maj a b c in
    let e := xor (xor a b) c in Digit (addCarry d x y) e

def add : BinaryNat → BinaryNat → BinaryNat
:= addCarry False

def mul : BinaryNat → BinaryNat → BinaryNat
| Falses _ := Falses
| (Digit x False) y := Digit (mul x y) False
| (Digit x True) y := add y (Digit (mul x y) False)

#check zero
#check succ zero
#reduce add (succ zero) (succ zero)
#reduce let a := add (succ zero) (succ zero) in mul a a 

end BinaryNat

#check BinaryNat.zero
