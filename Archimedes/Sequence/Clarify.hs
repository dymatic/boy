module Archimedes.Sequence.Clarify(
           contains
         , mostly
         , allEq
         , find) where

import Archimedes.Sequence.Functional
import Archimedes.Common
-- Local Functions
  
contains :: (Eq a)=> [a] -> [a] -> Bool
contains [] _ = False
contains x a
    | (take (length a) x) == a = True
    | otherwise = contains (tail x) a

mostly :: (Eq a) => [a] -> a -> Bool
mostly xs b = let times = (count xs b) in times > (length xs - times)

allEq :: (Eq a) => [a] -> a -> Bool
allEq xs b = (length xs) == (length $ filterBreak (==b) xs)

find :: (Eq a) => [(a,b)] -> a -> b
find a@((d,c):xs) b
  | (b `notElem` (map fst a)) = c
  | d == b = c
  | otherwise = find xs b

