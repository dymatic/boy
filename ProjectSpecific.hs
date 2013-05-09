module ProjectSpecific(
	titles
  , descriptions
  , allDescriptions
	) where

import LibHaskell.LibLists

titles :: [String] -> [String]
titles [] = []
titles (x:xs)
	| x `contains` "===" = (before (afterList x "===") ':'):titles xs
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
allDescriptions a@(x:xs) = rm (descriptions a : (allDescriptions (strt xs (specpos xs "-")))) [] 

twd :: [String] -> [(String,[String])]
twd x = sew (titles x) (allDescriptions x)

describe :: [String] -> String -> [String]
describe x y = look y (twd x) 

