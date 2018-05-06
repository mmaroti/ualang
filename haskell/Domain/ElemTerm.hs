module Domain.ElemTerm(ElemTerm(..)) where

import Domain.Constant as Constant

data ElemTerm
    = ConstTerm Constant
    | Composition ElemTerm [ElemTerm]
    deriving (Show, Eq)


    -- typecheck :: Term -> Domain
