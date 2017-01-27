module Lib ( runVM ) where

import qualified Data.Binary.Get      as G
import qualified Data.ByteString.Lazy as BS
import           Data.List.Split      (chunksOf)
import qualified Data.Vector          as V
import qualified Data.Word            as W

-- == architecture ==
-- three storage regions
data State = State { mem    :: V.Vector W.Word16 -- memory with 15-bit
                                                 -- address space
                                                 -- storing 16-bit
                                                 -- values
                   , reg    :: V.Vector W.Word16 -- eight registers
                   , stack  :: [W.Word16] -- an unbounded stack which
                                          -- holds individual 16-bit
                                          -- values
                   , pc     :: W.Word16
                   , outBuf :: Maybe Char
                   , inBuf  :: String
                   , status :: Status
                   }
    deriving (Eq, Show)

data Val = Lit W.Word16 | Reg Int

data Op = Halt
    | Out Val
    | Noop

-- all numbers are unsigned integers 0..32767 (15-bit)
-- all math is modulo 32768; 32758 + 15 => 5
maxVal :: W.Word16
maxVal = 32768

data Status = Running | Reading | Halted
    deriving (Eq, Show)

prog :: IO (V.Vector W.Word16)
prog = do
    f <- BS.readFile "challenge.bin"
    return $ V.fromList $ G.runGet getWords f

-- each number is stored as a 16-bit little-endian pair
-- (low byte, high byte)
getWords :: G.Get [W.Word16]
getWords = do
    empty <- G.isEmpty
    if empty
        then return []
        else do
            w <- G.getWord16le
            ws <- getWords
            return (w : ws)

readOp :: State -> W.Word16 -> (Op, W.Word16)
readOp = undefined

-- initState :: BS.ByteString -> State
-- initState prog = State (parseProg prog)
--                        []
--                        (V.fromList [ 0, 0, 0, 0, 0, 0, 0, 0 ])
--                        0
--                        Nothing
--                        []
--                        Running

runVM :: IO ()
runVM = undefined

stepVm :: State -> State
stepVm s = undefined
