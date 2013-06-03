module Archimedes.Tasking.Statistics(
    similarComposure
  , isSimilarWord
  , mostSimilarWord) where

import Archimedes.Sequence.Functional
import Archimedes.Sequence.Manipulate

import Archimedes.Tasking.Algorithm

-- LOCAL FUNCTIONS
refPos :: (Eq a) => [a] -> [b] -> a -> b
refPos a b c = b !! (pos a c)

score :: (Eq a) => [(a -> a -> Bool)] -> a -> a -> Int
score fl t1 t2 = length $ [x | x <- fl, (x t1 t2)]

-- EXPORTED FUNCTIONS
similarComposure :: (Eq a) => [a] -> [a] -> [a] -> Bool
similarComposure a b c = (abs (ws1 - ws2)) < 5
  where ws1 = sum $ map (refPos a [1..(length a)]) b
        ws2 = sum $ map (refPos a [1..(length a)]) c

evaluate :: String -> String -> Int
evaluate w1 w2 = (score [ses, (similarComposure all), similarLengths] w1 w2)
  where all = ['a'..'z'] ++ ['A'..'Z'] ++ ['1'..'3'] ++ "[{]}\\|`~!@#$%^&*()_+-=,<.>/?"
        
isSimilarWord :: String -> String -> Bool
isSimilarWord w1 w2 = (evaluate w1 w2) >= 2
  
mostSimilarWord :: String -> [String] -> String
mostSimilarWord word list = refPos evalList list most
  where evalList = (map (evaluate word) list)
        most = head $ sort evalList

-- Helper Functions
ses :: String -> String -> Bool
ses a b = and [(head a == head b),(last a == last b)]

similarLengths :: [a] -> [a] -> Bool
similarLengths a b = and [sc < l1, sc < l2]
  where l1 = (length a)
        l2 = (length b)
        sc = (abs (l1 - l2))
