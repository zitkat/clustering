module StirlingSpec where

import Clustering.Stirling
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = do
    describe "Stirling numbers and clustering purities" $ do
        stirlingNumTest stirling2


stirlingNumTest :: (Integer -> Integer -> Integer) -> SpecWith ()
stirlingNumTest s = describe "Stirling numbers" $ do
        it "s spec" $ do
            s 1 1 `shouldBe` 1
            s 3 2 `shouldBe` 3
            s 15 3 `shouldBe` 2375101
            s 20 4 `shouldBe` 45232115901
        it "s x 1" $ do
            property (\(NonNegative x) -> s (x :: Integer) 1 == 1)
        it "s x x" $ do
            property (\(NonNegative x) -> s (x :: Integer) (x :: Integer) == 1)
        it "s x 2" $ do
            property (\(NonNegative x) -> s (x :: Integer) 2 == rexp x)
        where 
            rexp x | x <= 0 = 0
            rexp x = 2^(x - 1) - 1
