module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad (forM_, replicateM)
import Control.Monad.State.Strict (execState, evalState)
import qualified Data.Sequence as Seq

import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f input output =
  it (show input ++ " ==> " ++ show output) $
    f input `shouldBe` output

spec :: Spec
spec = do
  describe "step" $ do
    let ex' = ex (execState (step 3))
    ex' (BufferState (Seq.singleton 0) 0 1) (BufferState (Seq.fromList [0, 1]) 1 2)
    ex' (BufferState (Seq.fromList [0, 1]) 1 2) (BufferState (Seq.fromList [0, 2, 1]) 1 3)
    ex' (BufferState (Seq.fromList [0, 2, 1]) 1 3) (BufferState (Seq.fromList [0, 2, 3, 1]) 2 4)
    -- it "just show me the state" $
      -- execState (replicateM 5 $ step 394) initialState `shouldBe` initialState
  describe "part1" $ do
    let ex' = ex part1
    ex' 3 638
  describe "insertionPositions" $ do
    let stepSize = 394
    it "is accurate to the positions from part1" $ property $
      \n -> n > 1 && n < 200 ==>
        let BufferState _ i value = execState (replicateM n (step stepSize)) initialState
            (i', value') = insertionPositions stepSize !! n
        in (i, value) === (i', value')
  describe "part2brute" $ do
    it "value after 0 always in position 1" $ property $
      \n -> n > 1 ==>
        let x = evalState (replicateM n (step 394) >> getAfter0) initialState
            x' = evalState (replicateM n (step 394) >> get1) initialState
        in x === x'
    -- let ex' = ex (\n -> part2brute n 394)
  --   ex' 0 0
  --   forM_ [1..2] $ \n -> ex' n 1
  --   forM_ [3..7] $ \n -> ex' n 3
  --   forM_ [8..22] $ \n -> ex' n 8
  --   forM_ [23..55] $ \n -> ex' n 23
  --   forM_ [56..164] $ \n -> ex' n 56
  --   forM_ [165..200] $ \n -> ex' n 165

  describe "part2" $ do
    it "has the same value as part2brute" $ property $
      \n -> n > 1 && n < 3000 ==>
        part2brute n 394 === part2 n 394
