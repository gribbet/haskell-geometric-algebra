module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           GeometricAlgebra

a = fromVector $ Vector 'a'
b = fromVector $ Vector 'b'
c = fromVector $ Vector 'c'

main :: IO ()
main = do
  defaultMain
    $ testGroup "Tests" [
      testCase "Anticommutivity" $
        a * b @?= -(b * a)
    , testCase "Associativity" $
        a * (b * c )@?= (a * b) * c
    ]
