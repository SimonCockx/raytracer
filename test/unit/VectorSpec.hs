module VectorSpec where

import Test.Hspec
import Test.Hspec.SmallCheck

import Vector
import Data.Massiv.Array as A

spec :: Spec
spec = parallel $ do 
    describe "Vector.normSqr" $ do
        it "equals the sum of the squared components" $ do
            property $ \x y z -> normSqr (Vector x y z) == (x*x + y*y + z*z :: Double)
    

    describe "Vector.normalize" $ do
        it "returns a vector with norm 1" $ do
            property $ \x y z ->
                if x == 0 && y == 0 && z == 0 then norm (normalize (Vector x y z)) == 0
                                              else norm (normalize (Vector x y z)) `closeTo` 1

closeTo :: Double -> Double -> Bool
closeTo x y = abs (x-y)/y < 1e-12
