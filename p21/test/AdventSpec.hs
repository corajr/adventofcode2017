module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad (forM_)
import Data.List (replicate)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Text.Parsec
import Advent

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

ex f input output =
  it (show input ++ " ==> " ++ show output) $
    f input `shouldBe` output

rule1 = "../.# => ##./#../..."
rule1parsed = Rule
  ["..", ".#"]
  ["##.", "#..", "..."]

rule2 = ".#./..#/### => #..#/..../..../#..#"
rule2parsed = Rule
  [".#.", "..#", "###"]
  ["#..#", "....", "....", "#..#"]

fourSplit =
  [ ["#."
    ,".."]
  , [".#"
    ,".."]
  , [".."
    ,"#."]
  , [".."
    ,".#"]
  ]

newGrid =
  [ "##.##."
  , "#..#.."
  , "......"
  , "##.##."
  , "#..#.."
  , "......"
  ]

exampleRules = unlines
  [ rule1
  , rule2
  ]

rotationStart =
  [ ".#."
  , "..#"
  , "###"
  ]

rotatedExamples = Set.fromList
  [ [ ".#."
    , "..#"
    , "###"
    ]
  , [ ".#."
    , "#.."
    , "###"
    ]
  , [ "#.."
    , "#.#"
    , "##."
    ]
  , [ "###"
    , "..#"
    , ".#."
    ]
  ]

spec :: Spec
spec = do
  describe "pRule" $ do
    let ex' = ex (parse pRule "")
    ex' rule1 (Right rule1parsed)
    ex' rule2 (Right rule2parsed)
  describe "rotations" $ do
    let rotated = rotations rotationStart
    it "produces rotations" $ do
      let xs = Set.fromList rotated
      rotatedExamples `shouldSatisfy` (`Set.isSubsetOf` xs)
  describe "concatByCol" $ do
    let ex' = ex concatByCol
    ex' [["ab", "ef"], ["cd", "gh"]] ["abcd", "efgh"]
  describe "arrangeByCol" $ do
    let ex' = ex arrangeByCol
    ex' [["#.",".#"],["..",".."]] [["#.", ".."], [".#", ".."]]
  describe "splitIntoTiles" $ do
    let ex2' = ex (splitIntoTiles 2)
        ex3' = ex (splitIntoTiles 3)
    ex2' (beforePat rule1parsed) [beforePat rule1parsed]
    ex2' (afterPat rule2parsed) fourSplit
    ex2' [ "abcd"
         , "efgh"
         , "hijk"
         , "lmop"
         ] $ [["ab", "ef"], ["cd", "gh"], ["hi", "lm"], ["jk", "op"]]
    ex3' (afterPat rule1parsed) [afterPat rule1parsed]
    ex3' (beforePat rule2parsed) [beforePat rule2parsed]
    ex3' newGrid (replicate 4 (afterPat rule1parsed))
    ex3' [ "abcdef"
         , "ghijkl"
         , "mnopqr"
         , "stuvwx"
         , "yzABCD"
         , "EFGHIJ"
         ] $ [["abc", "ghi", "mno"], ["def", "jkl", "pqr"], ["stu", "yzA", "EFG"], ["vwx", "BCD", "HIJ"]]
  describe "joinTiles" $ do
    let ex' = ex joinTiles
    ex' fourSplit (afterPat rule2parsed)
    ex' [["#..#","....","....","#..#"]] ["#..#","....","....","#..#"]
    ex' [["ab", "ef"], ["cd", "gh"], ["hi", "lm"], ["jk", "op"]] $
      [ "abcd"
      , "efgh"
      , "hijk"
      , "lmop"
      ]
    ex' [["abc", "ghi", "mno"], ["def", "jkl", "pqr"], ["stu", "yzA", "EFG"], ["vwx", "BCD", "HIJ"]] $
      [ "abcdef"
      , "ghijkl"
      , "mnopqr"
      , "stuvwx"
      , "yzABCD"
      , "EFGHIJ"
      ]

    ex' (replicate 4 (afterPat rule1parsed)) newGrid
  describe "part1" $ do
    let ex' = ex (part1 2)
    ex' exampleRules 12
