module LibHaskell.Math.LibGeometry(
  pythag
 ,pythagHypot) where

import LibHaskell.Math.LibAlgebra

pythag :: (Fractional a, Floating a) => (a,a) -> a
pythag (a,b) =  sqrt ((sqr a) + (sqr b))    --c2 = a2 + b2

pythagHypot :: (Fractional a, Floating a) => (a,a) -> a
pythagHypot (a,b) = sqrt ((sqr a) - (sqr b)) 