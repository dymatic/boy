module ProjectSpecific(
	titles
  , descriptions
  , allDescriptions
  , getInfo
  , specTitles
	) where

import Archimedes.Sequence.Manipulate
import Archimedes.Sequence.Clarify
import Archimedes.Sequence.Functional

look :: (Eq a) => [(a,b)] -> a -> b
look [] _ = error "Not in list"
look ((a,b):xs) c
  | a == c = b
  | otherwise = look xs c

titles :: [String] -> [String]
titles [] = []
titles (x:xs)
	| (take 5 x) `contains` "===" = (before (afterList x "===") ':'):titles xs
	| otherwise = titles xs

descriptions :: [String] -> [String]
descriptions [] = []
descriptions (x:xs)
	| x `contains` "===" = (filterBreak (\c -> c /= "-") xs)
	| otherwise = descriptions xs

specpos ::(Eq a) => [a] -> a -> Int
specpos a y
	| y `elem` a = pos a y
	| otherwise = length a

allDescriptions :: [String] -> [[String]]
allDescriptions [] = []
allDescriptions a@(x:xs) = rm (descriptions a : (allDescriptions (sub xs (specpos xs "-")))) [] 

twd :: [String] -> [(String,[String])]
twd x = zip (titles x) (allDescriptions x)

describe :: [String] -> String -> [String]
describe x y = look (twd x) y 

getInfo :: [String] -> String -> [String]
getInfo allLines title = look (twd allLines) title

specTitles :: [String] -> [String]
specTitles [] = []
specTitles (x:xs)
	| ((take 5 x) `contains` "===") = (afterList x "===") :  specTitles xs
	| otherwise = specTitles xs

