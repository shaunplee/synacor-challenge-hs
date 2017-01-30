module Verification where

import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy    as M
import           Debug.Trace

mkCycle :: Int -> (Int, Int) -> Int
mkCycle reg8 = let cache = M.fromList [ ((x, y), go (x, y))
                                      | x <- [0 .. 4] :: [Int]
                                      , y <- [0 .. 32768] :: [Int] ]
                   go (a, b) = if a == 0
                               then (b + 1) `mod` 32768
                               else if b == 0
                                    then cache M.! (a - 1, reg8)
                                    else cache M.! (a - 1, cache M.! (a, b - 1))
               in
                   if reg8 `mod` 100 == 0
                   then trace (show reg8) $ go
                   else go


run :: Int
run = head $ filter (\x -> mkCycle x (4, 1) == 6) [0..32767]
