module Base.Boolean (Boolean(..), not, and, or, xor, maj) where

import qualified Prelude

data Boolean = True | False
    deriving (Prelude.Show)

not :: Boolean -> Boolean
not True = False
not False = True

and :: Boolean -> Boolean -> Boolean
and False _ = False
and True y = y

or :: Boolean -> Boolean -> Boolean
or False y = y
or True _ = True

xor :: Boolean -> Boolean -> Boolean
xor False False = False
xor False True = True
xor True False = True
xor True True = False

maj :: Boolean -> Boolean -> Boolean -> Boolean
maj False = and
maj True = or
