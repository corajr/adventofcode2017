module Advent where

import Control.Monad (when, unless)
import Control.Monad.RWS.Strict
import Text.Regex.PCRE
import Data.List (find)
import Debug.Trace (traceShowId)
import qualified Data.Vector.Unboxed as V

type Firewall = V.Vector Int

data FirewallState = FirewallState
  { currentFirewall :: Firewall
  , position :: Int
  } deriving (Eq, Show, Ord)

type FirewallRWS = RWS Firewall [Int] FirewallState

parseFirewall :: String -> Firewall
parseFirewall xs = V.replicate n minBound V.// indices
  where
    indices = map parseScanner (lines xs)
    parseScanner :: String -> (Int, Int)
    parseScanner xs = let [_, x, y] = getAllTextSubmatches (xs =~ "(\\d+): (\\d+)")
                      in (read x, read y)
    n = 1 + (maximum $ map fst indices)

advanceLayers :: FirewallRWS ()
advanceLayers = do
  FirewallState currentFirewall _ <- get
  originalFirewall <- ask
  let f i a
        | range == minBound = minBound
        | a == range - 1 = negate a + 1
        | otherwise = a + 1
        where range = originalFirewall V.! i
      currentFirewall' = V.imap f currentFirewall
  modify (\st -> st { currentFirewall = currentFirewall' })

firewallStep :: Bool -> FirewallRWS ()
firewallStep exitEarly = do
  FirewallState currentFirewall position <- get
  originalFirewall <- ask
  let caught = currentFirewall V.! position == 0
      penalty = position * originalFirewall V.! position
      shouldStopEarly = caught && exitEarly
  when caught $ tell [penalty]
  unless (position == V.length originalFirewall - 1 || shouldStopEarly) $ do
    advanceLayers
    modify (\st -> st { position = position + 1})
    firewallStep exitEarly

initialState :: Firewall -> FirewallState
initialState original = FirewallState (V.map (const 0) original) 0

travelFirewall :: Bool -> Firewall -> FirewallState -> [Int]
travelFirewall exitEarly original state = w
  where (_, _, w) = runRWS (firewallStep exitEarly) original state

part1 :: String -> Int
part1 xs = sum $ travelFirewall False original startState
  where
    original = parseFirewall xs
    startState = initialState original

part2 :: String -> Int
part2 xs = maybe 0 snd $ find f (iterate g (startState, 0))
  where
    original = parseFirewall xs
    startState = initialState original
    iterate' f x = x `seq` x : iterate' f (f x)
    f (x, _) = null $ travelFirewall True original x
    g (x, i) = let (st, _) = execRWS advanceLayers original x
               in (st, i + 1)

cliMain :: IO ()
cliMain = do
  input <- readFile "../inputs/13.txt"
  print $ part2 input
