module Archimedes.Sequence.Functional(
	  filterBreak
        , count
        , unit) where

filterBreak :: (a -> Bool) -> [a] -> [a]
filterBreak _ [] = []
filterBreak f (x:xs)
	| f x = x : filterBreak f xs
	| otherwise = []

count :: (Eq a) => [a] -> a -> Int
count x c = sum $ [1 | f <- x, f == c]

unit :: [(a -> Bool)] -> a -> Int -> Bool
unit f b c = count [(x b) | x <- f] True >= c 
