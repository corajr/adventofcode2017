module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

part1example = unlines
  [ "5\t1\t9\t5",
    "7\t5\t3",
    "2\t4\t6\t8"
  ]

part2example = unlines
  [ "5\t9\t2\t8",
    "9\t4\t7\t3",
    "3\t8\t6\t5"
  ]

spec :: Spec
spec = do
  describe "parseTSVline" $ do
    it "parses a tab-separated line into a list of numbers" $
      parseTSVline "1\t2\t3\t4" `shouldBe` [1, 2, 3, 4]
  describe "checksum" $ do
    it "5 1 9 5 ==> 8" $
      checksum [5, 1, 9, 5] `shouldBe` 8
    it "7 5 3 ==> 4" $
      checksum [7, 5, 3] `shouldBe` 4
    it "2 4 6 8 ==> 6" $
      checksum [2, 4, 6, 8] `shouldBe` 6
  describe "part1" $ do
    it "should be 18 for the example" $
      part1 part1example `shouldBe` 18
  describe "divisible" $ do
    it "5 9 2 8 ==> 4" $
      divisible [5, 9, 2, 8] `shouldBe` 4
    it "9 4 7 3 ==> 3" $
      divisible [9, 4, 7, 3] `shouldBe` 3
    it "3 8 6 5 ==>  2" $
      divisible [3, 8, 6, 5] `shouldBe` 2
  describe "part2" $ do
    it "should be 9 for the example" $
      part2 part2example `shouldBe` 9

