module Tuple where

import Bool
import Prelude (Integer, Show, otherwise, undefined, ($), (*), (+), (-), (/), (>), (>=))

data Pair a b = Pair a b

a :: (Bool, Bool)
a = (True, True)

b :: (Bool, Integer)
b = (True, 0)

c :: ([Integer], Bool)
c = ([1], False)

d :: ((), Integer)
d = ((), 1)

e :: ((), ())
e = ((), ())

f :: (Bool, ((), Integer))
f = (True, ((), 1))

f' :: (Bool, (), Integer)
f' = (True, (), 1)

g :: (Bool, (), Bool, Integer)
g = (False, (), True, 0)

mod :: Integer -> Integer -> Integer
mod _ 0 = undefined
mod a b
  | a >= b = mod (a - b) b
  | otherwise = a

div :: Integer -> Integer -> Integer
div _ 0 = undefined
div a b
  | a >= b = div (a - b) b + 1
  | otherwise = 0

times :: Integer -> Integer -> Integer
times _ 0 = 0
times a b = a + times a (b -1)

divide :: Integer -> Integer -> (Integer, Integer)
divide a b
  | a >= b =
    let (x, y) = divide (a - b) b
     in (x, y + 1)
  | otherwise = (a, 0)

id :: a -> a
id x = x

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, x) = x

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

dup :: a -> (a, a)
dup x = (x, x)

map :: (b -> c) -> (a, b) -> (a, c)
map f (x, y) = (x, f y)

foo :: (a, b, c, a) -> (c, a)
foo (_, _, z, w) = (z, w)
foo (x, _, z, _) = (z, x)