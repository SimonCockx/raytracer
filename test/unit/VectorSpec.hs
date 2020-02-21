module VectorSpec where

import Test.Hspec
import Test.Hspec.SmallCheck

import Vector
import Data.Massiv.Array as A

spec :: Spec
spec = parallel $ do
    describe "Vector.createVector" $ do
        it "returns an array of size 4" $ do
            property $ \x y z -> size (createVector x y z) == Sz 4
    
        it "returns an array with last element 0" $ do
            property $ \x y z -> evaluate' (createVector x y z) 3 == 0
    

    describe "Vector.normSqr" $ do
        it "equals the sum of the squared components" $ do
            property $ \x y z -> normSqr (createVector x y z) == x*x + y*y + z*z
    

    describe "Vector.normalize" $ do
        it "returns a vector with norm 1" $ do
            property $ \x y z ->
                if x == 0 && y == 0 && z == 0 then norm (normalize (createVector x y z)) == 0
                                              else norm (normalize (createVector x y z)) `closeTo` 1

closeTo :: Double -> Double -> Bool
closeTo x y = abs (x-y)/y < 1e-12
