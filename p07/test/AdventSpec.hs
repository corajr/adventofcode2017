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
  [ "pbga (66)"
  , "xhth (57)"
  , "ebii (61)"
  , "havc (66)"
  , "ktlj (57)"
  , "fwft (72) -> ktlj, cntj, xhth"
  , "qoyq (66)"
  , "padx (45) -> pbga, havc, qoyq"
  , "tknk (41) -> ugml, padx, fwft"
  , "jptl (61)"
  , "ugml (68) -> gyxo, ebii, jptl"
  , "gyxo (61)"
  , "cntj (57)"
  ]

spec :: Spec
spec = do
  describe "pProgram" $ do
    it "parses a single leaf node" $
      parse pProgram "" "pbga (66)\n" `shouldBe` Right (Program "pbga" 66 [])
    it "parses a single node with children" $ do
      let input = "fwft (72) -> ktlj, cntj, xhth\n"
          output = Program "fwft" 72 ["ktlj", "cntj", "xhth"]
      parse pProgram "" input `shouldBe` Right output
  describe "pPrograms" $ do
    it "can parse a full example" $
      parse pPrograms "" ex1 `shouldSatisfy` isRight
  describe "part1" $ do
    it "returns 'tknk' for example" $
      part1 ex1 `shouldBe` "tknk"
  describe "part2" $ do
    it "returns 60 for example" $
      part2 ex1 `shouldBe` 60
