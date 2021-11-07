module Day where

import Prelude (Show)
import Bool

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show

weekend :: Day -> Bool
weekend Sat = True
weekend Sun = True
weekend _ = False

weekday :: Day -> Bool
weekday x = not (weekend x)