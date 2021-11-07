module RPS where

import Prelude (Show)

data RPS = Rock | Paper | Scissor deriving Show
data Result = Win | Lose | Draw deriving Show

showdown :: RPS -> RPS -> Result
showdown Rock Scissor = Win
showdown Paper Rock = Win
showdown Scissor Paper = Win
showdown Rock Rock = Draw
showdown Paper Paper = Draw
showdown Scissor Scissor = Draw
showdown _ _ = Lose