module MathLang where

data Domain
    = Union [Domain]
    | Product [Domain]
    | Power Domain Domain
    deriving (Show, Eq)

domainSize :: Domain -> Integer
domainSize dom = case dom of
    Union doms -> sum (map domainSize doms)
    Product doms -> product (map domainSize doms)
    Power dom1 dom2 -> domainSize dom1 ^ domainSize dom2

unitDomain :: Domain
unitDomain = Product []

smallDomain :: Int -> Domain
smallDomain num = Union (replicate num unitDomain)

boolDomain :: Domain
boolDomain = smallDomain 2

data Primitive
    = Unit
    | Cast Domain Domain
    | Project [Domain] Int
    | Inject [Domain] Int
    | Graph Domain Domain
    | Equal Domain
    deriving (Show, Eq)

constantType :: Primitive -> Domain
constantType lit = case lit of
    Unit -> Product []
    Cast dom1 dom2 -> Power dom1 dom2
    Project doms idx -> Power (doms !! idx) (Product doms)
    Inject doms idx -> Power (Union doms) (doms !! idx)
    Graph dom1 dom2 -> Power (Power boolDomain (Product [dom1, dom2])) (Power dom1 dom2)
    Equal dom -> Power boolDomain (Product [dom, dom])

data Term
    = Literal Primitive
    | Apply Term Term
    deriving (Show, Eq)

-- typecheck :: Term -> Domain
