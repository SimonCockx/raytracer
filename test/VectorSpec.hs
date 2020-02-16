module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Vector
import Data.Massiv.Array as A

main :: IO ()
main = do
    defaultMain (testGroup "Vector tests" [createVectorTest, normSqrTest, normalizeTest])

closeTo :: Double -> Double -> Bool
closeTo x y = abs (x-y)/y < 1e-12

createVectorTest :: TestTree
createVectorTest = testGroup "Test createVector"
    [ testProperty "Size should equal 4." (\x y z -> size (createVector x y z) == Sz 4)
    , testProperty "Last element should be 1." (\x y z -> evaluate' (createVector x y z) 3 == 1)
    ]

normSqrTest :: TestTree
normSqrTest = testGroup "Test normSqr"
    [ testProperty "Squared norm should be sum of squared components." (\x y z -> normSqr (createVector x y z) == x*x + y*y + z*z)
    ]

normalizeTest :: TestTree
normalizeTest = testGroup "Test normalize"
    [ testProperty "Normalized vector should have length 1." 
        (\x y z ->
            if x == 0 && y == 0 && z == 0 then norm (normalize (createVector x y z)) == 0
                                          else norm (normalize (createVector x y z)) `closeTo` 1)
    ]
