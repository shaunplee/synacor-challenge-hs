module Main where

import qualified Data.ByteString as BS
import           Lib

main :: IO ()
main = do
    Lib.runVM $ BS.readFile "challenge.bin"
