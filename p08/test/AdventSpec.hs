module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Advent
import Text.Parsec
import Data.Either (isRight)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


ex1 = unlines
  [ "b inc 5 if a > 1"
  , "a inc 1 if b < 5"
  , "c dec -10 if a >= 1"
  , "c inc -20 if c == 10"
  ]

spec :: Spec
spec = do
  describe "pCompareOp" $ do
    let ex inp out = it (inp ++ " ==> " ++ show out) $ parse pCompareOp "" inp `shouldBe` Right out
    ex ">" OP_GT
    ex "<" OP_LT
    ex ">=" OP_GE
    ex "<=" OP_LE
    ex "==" OP_EQ
  describe "pInstruction" $ do
    it "parses a single instruction" $
      parse pInstruction "" "b inc 5 if a > 1" `shouldBe` Right (Instruction "b" 5 ("a", OP_GT, 1))
  describe "pInstructions" $ do
    it "can parse a full example" $
      parse pInstructions "" ex1 `shouldSatisfy` isRight
  describe "part1" $ do
    it "returns 1 for example" $
      part1 ex1 `shouldBe` 1
