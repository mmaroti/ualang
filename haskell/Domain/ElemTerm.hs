module Domain.ElemTerm(ElemTerm(..)) where

import Domain.Constant as Constant

data ElemTerm
    = Const Constant
    | Tuple [ElemTerm]
    | Apply ElemTerm ElemTerm
    deriving (Show, Eq)


    -- typecheck :: Term -> Domain
