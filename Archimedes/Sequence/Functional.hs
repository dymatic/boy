module Archimedes.Sequence.Functional(
	  filterBreak
        , count
        , unit
        , removeBreak
        , first) where

filterBreak :: (a -> Bool) -> [a] -> [a]
filterBreak _ [] = []
filterBreak f (x:xs)
	| f x = x : filterBreak f xs
	| otherwise = []

removeBreak :: (a -> Bool) -> [a] -> [a]
removeBreak _ [] = []
removeBreak f (x:xs)
  | f x = removeBreak f xs
  | otherwise = (x:xs)

count :: (Eq a) => [a] -> a -> Int
count x c = sum $ [1 | f <- x, f == c]

unit :: [(a -> Bool)] -> a -> Int -> Bool
unit f b c = count [(x b) | x <- f] True >= c 

first :: (a -> Bool) -> [a] -> a
first x (b:bs) = if x b then b else first x bs
