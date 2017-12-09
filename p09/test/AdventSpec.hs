module AdventSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad (forM_)

import Advent
import Text.Parsec
import Data.Either (isRight, isLeft)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

exampleInvalidGarbages =
  [ "<"
  , "<random characters"
  , "<<<<"
  , "<{>}>"
  , "<!>"
  , "<!!!>"
  , "<!,<{i<a!>"
  ]

exampleGarbages =
  [ "<>"
  , "<random characters>"
  , "<<<<>"
  , "<{!>}>"
  , "<!!>"
  , "<!!!>>"
  , "<{o\"i!a,<{i<a>"
  ]

exampleScores =
  [ ("{}", 1)
  , ("{{{}}}", 6)
  , ("{{},{}}", 5)
  , ("{{{},{},{{}}}}", 16)
  , ("{<a>,<a>,<a>,<a>}", 1)
  , ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9)
  , ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9)
  , ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)
  ]

spec :: Spec
spec = do
  describe "pGarbage" $ do
    let pGarbage' = pGarbage <* eof
    forM_ exampleInvalidGarbages $ \ex ->
      it ("does not consume all of " ++ ex) $
        parse pGarbage' "" ex `shouldSatisfy` isLeft
    forM_ exampleGarbages $ \ex ->
      it ("consumes all of " ++ ex) $
        parse pGarbage' "" ex `shouldBe` Right Garbage
  describe "part1" $ do
    forM_ exampleScores $ \(ex, n) ->
      it (ex ++ " ==> " ++ show n) $
        part1 ex `shouldBe` n
