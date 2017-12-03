module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getDisplacement" $ do
    it "1 ==> (0,0)" $
      getDisplacement 1 `shouldBe` (0, 0)
    it "2 ==> (1,0)" $
      getDisplacement 2 `shouldBe` (1, 0)
    it "3 ==> (1,1)" $
      getDisplacement 3 `shouldBe` (1, 1)
    it "4 ==> (0,1)" $
      getDisplacement 4 `shouldBe` (0, 1)
    it "5 ==> (-1,1)" $
      getDisplacement 5 `shouldBe` (-1, 1)
    it "6 ==> (-1,0)" $
      getDisplacement 6 `shouldBe` (-1, 0)
    it "7 ==> (-1,-1)" $
      getDisplacement 7 `shouldBe` (-1, -1)
    it "8 ==> (0,-1)" $
      getDisplacement 8 `shouldBe` (0, -1)
    it "9 ==> (1,-1)" $
      getDisplacement 9 `shouldBe` (1, -1)
    it "10 ==> (2,-1)" $
      getDisplacement 10 `shouldBe` (2, -1)
    it "11 ==> (2,0)" $
      getDisplacement 11 `shouldBe` (2, 0)
    it "12 ==> (2,1)" $
      getDisplacement 12 `shouldBe` (2, 1)
  describe "manhattanDist" $ do
    it "1 ==> 0" $
      manhattanDist 1 `shouldBe` 0
    it "2 ==> 1" $ do
      manhattanDist 2 `shouldBe` 1
    it "4 ==> 1" $ do
      manhattanDist 4 `shouldBe` 1
    it "6 ==> 1" $ do
      manhattanDist 6 `shouldBe` 1
    it "8 ==> 1" $ do
      manhattanDist 8 `shouldBe` 1
    it "3 ==> 2" $ do
      manhattanDist 3 `shouldBe` 2
    it "5 ==> 2" $ do
      manhattanDist 5 `shouldBe` 2
    it "7 ==> 2" $ do
      manhattanDist 7 `shouldBe` 2
    it "7 ==> 2" $ do
      manhattanDist 9 `shouldBe` 2
    it "12 ==> 3" $
      manhattanDist 12 `shouldBe` 3
    it "23 ==> 2" $
      manhattanDist 23 `shouldBe` 2
    it "1024 ==> 31" $
      manhattanDist 1024 `shouldBe` 31
  describe "adjSum" $ do
    it "1 ==> 1" $
      adjSum 1 `shouldBe` 1
    it "2 ==> 1" $
      adjSum 2 `shouldBe` 1
    it "3 ==> 2" $
      adjSum 3 `shouldBe` 2
    it "4 ==> 4" $
      adjSum 4 `shouldBe` 4
    it "5 ==> 5" $
      adjSum 5 `shouldBe` 5
    it "6 ==> 10" $
      adjSum 6 `shouldBe` 10
    it "7 ==> 11" $
      adjSum 7 `shouldBe` 11
