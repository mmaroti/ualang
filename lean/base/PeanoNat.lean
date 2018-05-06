
inductive PeanoNat
| zero : PeanoNat
| succ (n : PeanoNat) : PeanoNat

namespace PeanoNat

def add : PeanoNat → PeanoNat → PeanoNat
| x zero     := x
| x (succ y) := succ (add x y)

def mul : PeanoNat → PeanoNat → PeanoNat
| zero _     := zero
| (succ x) y := add y (mul x y)

#check zero
#check succ zero
#reduce add (succ zero) (succ zero)
#reduce let a := add (succ zero) (succ zero) in mul a a 

end PeanoNat

#check PeanoNat.zero
