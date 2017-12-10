module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Advent
import qualified Data.Vector.Unboxed as V

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "reverseFrom" $ do
    it "always returns input when length is 1" $ property $
      \xs i -> i > 0 && i < length xs ==>
        let xs' = V.fromList xs in reverseFrom i 1 xs' === xs'
    it "reverseFrom 0 3 [0,1,2,3,4] == [2,1,0,3,4]" $
      reverseFrom 0 3 (V.fromList [0..4]) `shouldBe` V.fromList [2,1,0,3,4]
    it "reverseFrom 3 4 [2,1,0,3,4] == [4,3,0,1,2]" $
      reverseFrom 3 4 (V.fromList [2,1,0,3,4]) `shouldBe` V.fromList [4,3,0,1,2]
    it "reverseFrom 3 1 [4,3,0,1,2] == [4,3,0,1,2]" $
      reverseFrom 3 1 (V.fromList [4,3,0,1,2]) `shouldBe` V.fromList [4,3,0,1,2]
    it "reverseFrom 1 5 [4,3,0,1,2] == [3,4,2,1,0]" $
      reverseFrom 1 5 (V.fromList [4,3,0,1,2]) `shouldBe` V.fromList [3,4,2,1,0]
  describe "part1" $ do
    it "part1 5 [3, 4, 1, 5] ==> 12" $
      part1 5 [3, 4, 1, 5] `shouldBe` 12
