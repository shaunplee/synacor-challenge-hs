module Lib ( runVm ) where

import qualified Data.Binary.Get      as G
import qualified Data.ByteString.Lazy as BS
import           Data.Char            (chr, ord)
import           Data.List.Split      (chunksOf)
import qualified Data.Vector          as V
import qualified Data.Word            as W
import           Debug.Trace

-- == architecture ==
-- three storage regions
data State = State { mem    :: V.Vector Int -- memory with 15-bit address space
                                            -- storing 16-bit values
                   , reg    :: V.Vector Int -- eight registers
                   , stack  :: [Int] -- an unbounded stack which holds
                                     -- individual 16-bit values
                   , pc     :: Int
                   , outBuf :: Maybe Char
                   , inBuf  :: String
                   , status :: Status
                   }
    deriving (Eq, Show)

data Val = Lit Int | Reg Int
    deriving (Eq, Show)

data Op = Halt
        | Set Val Val
        | Jmp Val
        | Jt Val Val
        | Jf Val Val
        | Out Val
        | Noop
    deriving (Eq, Show)

-- all numbers are unsigned integers 0..32767 (15-bit)
-- all math is modulo 32768; 32758 + 15 => 5
maxLit :: Int
maxLit = 32768

data Status = Running | Reading | Halted
    deriving (Eq, Show)

prog :: IO (V.Vector Int)
prog = do
    f <- BS.readFile "challenge.bin"
    return $ V.fromList $ G.runGet getWords f

-- each number is stored as a 16-bit little-endian pair
-- (low byte, high byte)
getWords :: G.Get [Int]
getWords = do
    empty <- G.isEmpty
    if empty
        then return []
        else do
            w <- G.getWord16le
            ws <- getWords
            return (fromEnum w : ws)

readOp :: State -> Int -> (Op, Int)
readOp s pc = let m = mem s in
    case m V.! pc of
        0         -> (Halt, 1)
        1         -> (Set (parseVal s 1) (parseVal s 2), 3)
        6         -> (Jmp $ parseVal s 1, 0)
        7         -> (Jt (parseVal s 1) (parseVal s 2), 3)
        8         -> (Jf (parseVal s 1) (parseVal s 2), 3)
        19        -> (Out $ parseVal s 1, 2)
        21        -> (Noop, 1)
        otherwise -> error (show $ m V.! pc)

parseVal :: State -> Int -> Val
parseVal (State m _ _ p _ _ _) i =
    let v = m V.! (p + i) in
        if v < maxLit then Lit v
        else Reg $ v - maxLit

decodeVal :: State -> Val -> Int
decodeVal _ (Lit i)                      = i
decodeVal (State _ rs _ _ _ _ _) (Reg i) = rs V.! i

initState :: V.Vector Int -> State
initState prog = State prog
                       (V.fromList [ 0, 0, 0, 0, 0, 0, 0, 0 ])
                       []
                       0
                       Nothing
                       []
                       Running

setPc :: State -> Int -> State
setPc (State m r st _ o i s) j =
    State m r st j o i s

incPc :: State -> Int -> State
incPc s j = setPc s (j + pc s)

setReg :: State -> Val -> Int -> State
setReg (State m rs st p o i s) (Reg r) v =
    State m (rs V.// [(r, v)]) st p o i s

setStatus :: State -> Status -> State
setStatus (State m r st p o i _) s =
    State m r st p o i s

setOut :: State -> Int -> State
setOut (State m r st p _ i s) o =
    State m r st p (Just $ chr o) i s

clearOut :: State -> State
clearOut (State m r st p _ i s) =
    State m r st p Nothing i s

setInput :: State -> String -> State
setInput (State m r st p o _ s) i =
    State m r st p o i s

execOp :: State -> (Op, Int) -> State
execOp s (Halt, n)  = setStatus s Halted
execOp s (Set a b, n) = incPc (setReg s a (decodeVal s b)) n
execOp s (Jmp v, _) = setPc s (decodeVal s v)
execOp s (Jt a b, n) = if decodeVal s a /= 0
                       then setPc s (decodeVal s b)
                       else incPc s n
execOp s (Jf a b, n) = if decodeVal s a == 0
                       then setPc s (decodeVal s b)
                       else incPc s n
execOp s (Out c, n) = setOut (incPc s n) (decodeVal s c)
execOp s (Noop, n)  = incPc s n
execOp _ op         = error (show op)

stepVm :: State -> State
stepVm s = execOp s (readOp s (pc s))

runVm :: IO ()
runVm = do
    p <- prog
    go (initState p)
    where
        go s = case status s of
            Halted -> return ()
            Running -> let ns = case outBuf s of
                               Just c -> do
                                   putChar c
                                   return $ clearOut s
                               Nothing -> return s
                       in
                           do
                               a <- ns
                               go $ stepVm a
            Reading -> do
                l <- getLine
                go $ setInput s l
