module Main where

import           GeometricAlgebra

-- Generic Vectors?

a = fromVector $ Vector 'a'
b = fromVector $ Vector 'b'


main :: IO ()
main = putStrLn $ "Test: " ++ show (a * b)
