module Main where

import           Lib
import           Verification

main :: IO ()
main = do
    Lib.runVm
--    putStrLn $ show Verification.run
