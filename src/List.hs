module List where

import Data.Bool

import GHC.Show
import Tuple
import Prelude (Eq, Integer, String, otherwise, undefined, ($), (+), (-), (.),  (/=), (<), (==), (*))

type Sequence = [Integer]

a :: [a]
a = []

b :: Sequence
b = [1]

c :: Sequence
c = [2]

d :: Sequence
d = [3, 3]

e :: Sequence
e = [1, 2, 3, 4, 5]

head :: [a] -> a
head (x : _) = x
head _ = undefined

headDef :: a -> [a] -> a
headDef _ (x : _) = x
headDef x [] = x

null :: [a] -> Bool
null [] = True
null _ = False

tail :: [a] -> [a]
tail (_ : xs) = xs
tail _ = undefined

last :: [a] -> a
last [] = undefined
last [x] = x
last (x : xs) = last xs

length :: [a] -> Integer
length [] = 0
length (x : xs) = length xs + 1

sum :: Sequence -> Integer
sum [] = 0
sum (x : xs) = sum xs + x

drop :: Integer -> [a] -> [a]
drop _ [] = []
drop 0 x = x
drop n (x : xs) = drop (n -1) xs

take :: Integer -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x : xs) = x : take (n -1) xs

min :: Sequence -> Integer
min [] = undefined
min [x] = x
min (x : xs)
  | min xs < x = min xs
  | otherwise = x

max :: Sequence -> Integer
max [] = undefined
max [x] = x
max (x : xs)
  | max xs < x = x
  | otherwise = max xs

skip :: [a] -> [a]
skip [] = []
skip [x] = [x]
skip (x : _ : xs) = x : skip xs

skip' :: [a] -> [a]
skip' [] = []
skip' [x] = []
skip' (_ : x : xs) = x : skip' xs

(++) :: [a] -> [a] -> [a]
x ++ [] = x
[] ++ y = y
(x : xs) ++ y = x : (xs ++ y)

init :: [a] -> [a]
init [] = undefined
init [x] = []
init (x : xs) = x : init xs

concat = (++)

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs `concat` [x]

remove :: Sequence -> Integer -> Sequence
remove [] _ = []
remove (x : xs) a
  | x == a = remove xs a
  | otherwise = x : remove xs a

removeOne :: Sequence -> Integer -> Sequence
removeOne [] _ = []
removeOne (x : xs) a
  | x == a = xs
  | otherwise = x : removeOne xs a

takeMin :: Sequence -> (Integer, Sequence)
takeMin [] = undefined
takeMin [x] = (x, [])
takeMin (x : xs)
  | x < y = (x, y : ys)
  | otherwise = (y, x : ys)
  where
    (y, ys) = takeMin xs

sort [] = []
sort xs =
  let (y, ys) = takeMin xs
   in y : sort ys

zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip [] _ = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x, y) : xys) =
  let (xs, ys) = unzip xys
      (x : xs, y : ys)

add' :: Integer -> Integer -> Integer
add' x y = x + y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs)
  | f x = x : filter f xs
  | otherwise = filter f xs