module LibHaskell.Math.LibAlgebra(
      avg
	, sqr
	, stdDev
	) where
import Data.List

avg :: (Real a, Fractional b) => [a] -> b
avg xs = realToFrac (sum xs) / genericLength xs

sqr x = x * x


stdDev :: (Fractional t, Real t, Floating t) => [t] -> t
stdDev xs = let ls = [sqr(mean - x)| x <- xs, let mean = avg xs]
			in sqrt $ realToFrac (sum ls) / realToFrac ((length xs) - 1)