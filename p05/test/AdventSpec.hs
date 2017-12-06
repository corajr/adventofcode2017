module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Advent
import qualified Data.Vector.Unboxed as V

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex1 = unlines ["0", "3", "0", "1", "-3"]

spec :: Spec
spec = do
  describe "parseInstructions" $ do
    it "should parse a set of instructions" $
      parseInstructions ex1 `shouldBe` V.fromList [0, 3, 0, 1, -3]
  describe "part1" $ do
    it "[0, -1] ==> 4" $
      part1 "0\n-1\n" `shouldBe` 4
    it "[0, 3, 0, 1, -3] ==> 5" $
      part1 ex1 `shouldBe` 5
  describe "part2" $ do
    it "[0, 3, 0, 1, -3] ==> 10" $
      part2 ex1 `shouldBe` 10
