module Domain.Domain(Domain(..), size, unit, boolean, small) where

data Domain
    = Union [Domain]
    | Product [Domain]
    | Power Domain [Domain]
    deriving (Show, Eq)

size :: Domain -> Integer
size dom = case dom of
    Union doms -> sum (map size doms)
    Product doms -> product (map size doms)
    Power dom1 dom2 -> size dom1 ^ size dom2

unit :: Domain
unit = Product []

small :: Int -> Domain
small num = Union (replicate num unit)

boolean :: Domain
boolean = small 2
