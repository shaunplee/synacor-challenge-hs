-- the ans

module Verification where

import qualified Data.IntMap.Lazy as IM
import           Debug.Trace

mkCycle :: Int -> (Int, Int) -> Int
mkCycle reg8 = let cache = [ IM.fromList [ (y, go (0, y))
                                         | y <- [0 .. 32767] :: [Int] ]
                           , IM.fromList [ (y, go (1, y))
                                         | y <- [0 .. 32767] :: [Int] ]
                           , IM.fromList [ (y, go (2, y))
                                         | y <- [0 .. 32767] :: [Int] ]
                           , IM.fromList [ (y, go (3, y))
                                         | y <- [0 .. 32767] :: [Int] ]
                           , IM.fromList [ (y, go (4, y))
                                         | y <- [0 .. 32767] :: [Int] ]
                           ]
                   go (a, b)
                       | a == 0 = (b + 1) `mod` 32768
                       | b == 0 = (cache !! (a - 1)) IM.! reg8
                       | otherwise = (cache !! (a - 1)) IM.!
                             (cache !! a IM.! (b - 1))
               in
                   go


run :: Int
run = head $ filter (\x -> mkCycle x (4, 1) == 6) [9000..32767]
-- run = 25734
