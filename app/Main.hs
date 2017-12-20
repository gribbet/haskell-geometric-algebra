module Main where

import           Data.List
import           GeometricAlgebra

basis :: [Multivector Char Int]
basis = map fromVectors ["", "a", "b", "ab"]

products = intercalate "\n" $ map (\a -> intercalate "\t" $ map show $ map (\b -> a * b) basis) basis

main :: IO ()
main = putStrLn products
