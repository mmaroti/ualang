module Domain.Constant(Constant(..), domainOf) where

import Domain.Domain as Domain

data Constant
    = Unit
    | Cast Domain Domain
    | Project [Domain] Int
    | Inject [Domain] Int
    | Graph Domain Domain
    | Equal Domain
    deriving (Show, Eq)

domainOf :: Constant -> Domain
domainOf Unit = Product []
domainOf (Cast dom1 dom2) = Power dom1 dom2
domainOf (Project doms idx) = Power (doms !! idx) (Product doms)
domainOf (Inject doms idx) = Power (Union doms) (doms !! idx)
domainOf (Graph dom1 dom2) = Power (Power Domain.boolean (Product [dom1, dom2])) (Power dom1 dom2)
domainOf (Equal dom) = Power Domain.boolean (Product [dom, dom])
