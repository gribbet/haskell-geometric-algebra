module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           GeometricAlgebra

a = fromVector 'a'
b = fromVector 'b'
c = fromVector 'c'

main :: IO ()
main = do
  defaultMain
    $ testGroup "Tests" [
        testCase "Anticommutivity" $
          a * b @?= -(b * a)
      , testCase "Associativity" $
          a * (b * c) @?= (a * b) * c
      , testCase "Show" $
          show (a * b * c) @?= "abc"
    ]
