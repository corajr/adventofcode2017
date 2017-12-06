module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Advent
import qualified Data.Vector.Unboxed as V

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex1 = "0\t2\t7\t0"

v1 = V.fromList [0, 2, 7, 0]
v2 = V.fromList [2, 4, 1, 2]

spec :: Spec
spec = do
  describe "step" $ do
    it "takes [0,2,7,0] ==> [2,4,1,2]" $
      step v1 `shouldBe` v2
  describe "part1" $ do
    it "[0, 2, 7, 0] ==> 5" $
      part1 ex1 `shouldBe` 5
  describe "part2" $ do
    it "[0, 2, 7, 0] ==> 4" $
      part2 ex1 `shouldBe` 4
