module MathLang where

data Domain
    = Unit
    | Product Domain Domain
    | Zero
    | Union Domain Domain
    | Power Domain Domain
    deriving (Show, Eq)

domainSize :: Domain -> Integer
domainSize dom = case dom of
    Unit -> 1
    Product dom1 dom2 -> domainSize dom1 * domainSize dom2
    Zero -> 0
    Union dom1 dom2 -> domainSize dom1 + domainSize dom2
    Power dom1 dom2 -> domainSize dom1 ^ domainSize dom2

domainBits :: Domain -> Integer
domainBits dom = case dom of
    Unit -> 0
    Product dom1 dom2 -> domainBits dom1 + domainBits dom2
    Zero -> 0
    Union dom1 dom2 -> 1 + domainBits dom1 + domainBits dom2
    Power dom1 dom2 -> domainBits dom1 * domainSize dom2

data Term
    = UnitTerm
    | Pair Term Term
    | Embed1 Term Domain
    | Embed2 Domain Term
    | Apply Term Term
    | Cast Domain Term
    deriving (Show, Eq)

-- Product (Power a b1) (Power a b2) = Power a (Union b1 b2)
-- Product (Power a1 b) (Power a2 b) = Power (Product a1 a2) b

typeCheck :: Term -> Either String Domain
typeCheck term = case term of
    UnitTerm -> Right Unit
    Pair term1 term2 -> case typeCheck term1 of
        Left err -> Left err
        Right dom1 -> case typeCheck term2 of
            Left err -> Left err
            Right dom2 -> Right (Product dom1 dom2)
    Embed1 term1 dom2 -> case typeCheck term1 of
        Left err -> Left err
        Right dom1 -> Right (Union dom1 dom2)
    Embed2 dom1 term2 -> case typeCheck term2 of
        Left err -> Left err
        Right dom2 -> Right (Union dom1 dom2)
    Apply fun arg -> case typeCheck fun of
        Left err -> Left err
        Right fdom -> case fdom of
            Power adom rdom -> case typeCheck arg of
                Left err -> Left err
                Right dom -> if adom == dom
                    then Right rdom
                    else Left "invalid argument"
            _ -> Left "invalid function"
    Cast _ _ -> undefined

class Boolean elem where
    true ::elem
    false :: elem
    not :: elem -> elem
    and :: elem -> elem -> elem
    or :: elem -> elem -> elem

instance Boolean Bool where
    true = True
    false = False
    not = Prelude.not
    and = (&&)
    or = (||)

