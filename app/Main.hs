module Main where

import           GeometricAlgebra

a = fromVector 'a'
b = fromVector 'b'


main :: IO ()
main = putStrLn $ "Test: " ++ show (a * b)
