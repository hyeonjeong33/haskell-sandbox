module Maybe where

import List (length)
import Prelude (Show, undefined)

data Maybe a = Just a | Nothing deriving (Show)

extract :: Maybe a -> a
extract a = 
    case a of 
        Just a -> a
        Nothing -> undefined

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
        
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (x:xs) =
    case x of 
        Nothing -> removeNothing xs
        Just x' -> x' : removeNothing xs
