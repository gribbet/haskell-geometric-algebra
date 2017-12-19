module Main where

import           GeometricAlgebra

a :: Multivector
a = fromVector $ Vector 'a'

b :: Multivector
b = fromVector $ Vector 'b'

c :: Multivector
c = fromVector $ Vector 'c'

test = fromString "bac"

main :: IO ()
main = putStrLn $ "Test: " ++ show test
