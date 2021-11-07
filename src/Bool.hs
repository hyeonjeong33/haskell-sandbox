module Bool where

import Prelude (Show)

data Bool = True | False 
  deriving Show

not :: Bool -> Bool
not True = False
not False = True

and :: Bool -> Bool -> Bool
and True True = True

and _ _ = False

or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True

xor :: Bool -> Bool -> Bool
xor False False = False
xor True True = False
xor _ _ = True
