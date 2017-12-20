module Main where

import           GeometricAlgebra

basis :: [Multivector Char Int]
basis = map fromVectors ["", "a", "b", "ab"]

products = foldl (\x y -> x ++ "\n" ++ y) "" $ map (\a -> foldl (\x y -> x ++ "\t" ++ (show y)) "" $ map (\b -> a * (GeometricAlgebra.reverse b)) basis) basis

main :: IO ()
main = putStrLn products
