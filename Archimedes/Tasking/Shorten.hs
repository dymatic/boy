module Archimedes.Tasking.Shorten(
    mass
  , unmass) where

import Archimedes.Sequence.Manipulate
import Archimedes.Sequence.Functional
import Archimedes.Sequence.Clarify

import Archimedes.Common

mass :: String -> String
mass [] = []
mass (x:"") = x:""
mass a@(x:y:xs)
	| x == y = ("(" ++ (show rl) ++ [x] ++ ")") ++ mass (removeBreak (== x) a)
        | otherwise = x : mass (y:xs)
  where rl = length $ (filterBreak (== x) a)

unmass :: String -> String
unmass [] = []
unmass (x:"") = x:""
unmass a@(x:y:ys)
  | x == '(' = take nOf (repeat (last (before a ')'))) ++ unmass (after a ')')
  | otherwise = x : unmass (y:ys)
    where nOf =  (read $ tail (init ((before a ')')))) :: Int
	
