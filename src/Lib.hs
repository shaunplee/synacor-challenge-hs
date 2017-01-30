module Lib ( runVm ) where

import qualified Data.Binary.Get      as G
import           Data.Bits
import qualified Data.ByteString.Lazy as BS
import           Data.Char            (chr, ord)
import           Data.Serialize
import qualified Data.Vector.Unboxed  as V
import           Debug.Trace
import           System.IO            (hFlush, stdout)


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
    deriving (Eq, Show, Read)

data Val = Lit Int | Reg Int
    deriving (Eq, Show)

data Op = Halt
        | Set Val Val
        | Push Val
        | Pop Val
        | Equ Val Val Val
        | Gt Val Val Val
        | Jmp Val
        | Jt Val Val
        | Jf Val Val
        | Add Val Val Val
        | Mult Val Val Val
        | Mod Val Val Val
        | And Val Val Val
        | Or Val Val Val
        | Not Val Val
        | Rmem Val Val
        | Wmem Val Val
        | Call Val
        | Ret
        | Out Val
        | In Val
        | Noop
        | Data Val
    deriving (Eq, Show)

-- all numbers are unsigned integers 0..32767 (15-bit)
-- all math is modulo 32768; 32758 + 15 => 5
maxLit :: Int
maxLit = 32768

data Status = Running | Reading | Halted
    deriving (Eq, Show, Read)

prog :: IO (V.Vector Int)
prog = do
    f <- BS.readFile "challenge.bin"
    let p = V.fromList $ G.runGet getWords f
    -- the binary doesn't take up the full memory, so zero pad to get
    -- the full 15 bit address space
    return $ p V.++ V.replicate (maxLit - V.length p) 0

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
readOp s p = let m = mem s in
    case m V.! p of
        0  -> (Halt, 1)
        1  -> (Set (parseVal s p 1) (parseVal s p 2), 3)
        2  -> (Push (parseVal s p 1), 2)
        3  -> (Pop (parseVal s p 1), 2)
        4  -> (Equ (parseVal s p 1) (parseVal s p 2) (parseVal s p 3), 4)
        5  -> (Gt (parseVal s p 1) (parseVal s p 2) (parseVal s p 3), 4)
        6  -> (Jmp $ parseVal s p 1, 2)
        7  -> (Jt (parseVal s p 1) (parseVal s p 2), 3)
        8  -> (Jf (parseVal s p 1) (parseVal s p 2), 3)
        9  -> (Add (parseVal s p 1) (parseVal s p 2) (parseVal s p 3), 4)
        10 -> (Mult (parseVal s p 1) (parseVal s p 2) (parseVal s p 3), 4)
        11 -> (Mod (parseVal s p 1) (parseVal s p 2) (parseVal s p 3), 4)
        12 -> (And (parseVal s p 1) (parseVal s p 2) (parseVal s p 3), 4)
        13 -> (Or (parseVal s p 1) (parseVal s p 2) (parseVal s p 3), 4)
        14 -> (Not (parseVal s p 1) (parseVal s p 2), 3)
        15 -> (Rmem (parseVal s p 1) (parseVal s p 2), 3)
        16 -> (Wmem (parseVal s p 1) (parseVal s p 2), 3)
        17 -> (Call (parseVal s p 1), 2)
        18 -> (Ret, 1)
        19 -> (Out $ parseVal s p 1, 2)
        20 -> (In $ parseVal s p 1, 2)
        21 -> (Noop, 1)
        _  -> (Data $ parseVal s p 1, 1)

-- numbers 0..32767 mean a literal value
-- numbers 32768..32775 instead mean registers 0..7
parseVal :: State -> Int -> Int -> Val
parseVal (State m _ _ _ _ _ _) p i =
    let v = m V.! (p + i) in
        if v < maxLit then Lit v
        else Reg $ v `mod` maxLit

deVal :: State -> Val -> Int
deVal _ (Lit i)                      = i
deVal (State _ rs _ _ _ _ _) (Reg i) = rs V.! i

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

push :: State -> Int -> State
push (State m r st p o i s) v =
    State m r (v : st) p o i s

pop :: State -> (State, Int)
pop (State m r (v:st) p o i s) =
    (State m r st p o i s, v)

rmem :: State -> Val -> Int -> State
rmem (State m r st p o i s) (Reg a) from = State m nr st p o i s
    where nr = r V.// [(a, m V.! from)]

wmem :: State -> Int -> Int -> State
wmem (State m r st p o i s) to v = State nm r st p o i s
    where nm = m V.// [(to, v)]

setOut :: State -> Int -> State
setOut (State m r st p _ i s) o =
    State m r st p (Just $ chr o) i s

clearOut :: State -> State
clearOut (State m r st p _ i s) =
    State m r st p Nothing i s

readChar :: State -> Val -> State
readChar (State m rs st p o (i:is) s) (Reg r) =
    State m (rs V.// [(r, ord i)]) st p o is s

setInput :: State -> String -> State
setInput (State m r st p o _ s) i =
    State m r st p o i s

execOp :: State -> (Op, Int) -> State
execOp s (Halt, n)  = setStatus s Halted
execOp s (Set a b, n) = incPc (setReg s a (deVal s b)) n
execOp s (Push a, n) = incPc (push s (deVal s a)) n
execOp s (Pop a, n) = let (ns, b) = pop s in
                         incPc (setReg ns a b) n
execOp s (Equ a b c, n) =
    incPc (setReg s a (if (deVal s b) == (deVal s c) then 1 else 0)) n
execOp s (Gt a b c, n) =
    incPc (setReg s a (if (deVal s b) > (deVal s c) then 1 else 0)) n
execOp s (Jmp v, _) = setPc s (deVal s v)
execOp s (Jt a b, n) = if deVal s a /= 0
                       then setPc s (deVal s b)
                       else incPc s n
execOp s (Jf a b, n) = if deVal s a == 0
                       then setPc s (deVal s b)
                       else incPc s n
execOp s (Add a b c, n) =
    incPc (setReg s a ((deVal s b + deVal s c) `mod` maxLit)) n
execOp s (Mult a b c, n) =
    incPc (setReg s a ((deVal s b * deVal s c) `mod` maxLit)) n
execOp s (Mod a b c, n) =
    incPc (setReg s a (deVal s b `mod` deVal s c)) n
execOp s (And a b c, n) = incPc (setReg s a (deVal s b .&. deVal s c)) n
execOp s (Or a b c, n) = incPc (setReg s a (deVal s b .|. deVal s c)) n
execOp s (Not a b, n) = let neg = complement (deVal s b) .&. 32767
                        in
                            incPc (setReg s a neg) n
execOp s (Rmem a b, n) = incPc (rmem s a (deVal s b)) n
execOp s (Wmem a b, n) = incPc (wmem s (deVal s a) (deVal s b)) n
execOp s (Call a, n) = let t = deVal s a in
                           setPc (push s (n + pc s)) t
execOp s (Ret, n) = if null $ stack s
                    then setStatus s Halted
                    else let (ns, b) = pop s
                         in
                             setPc ns b
execOp s (Out c, n) = setOut (incPc s n) (deVal s c)
execOp s (In c, n) = if null $ inBuf s
                     then setStatus s Reading
                     else incPc (readChar s c) n
execOp s (Noop, n)  = incPc s n

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
                   in do
                       a <- ns
                       go $ stepVm a
        Reading -> do
            l <- getLine
            case l of
                ":debug" -> do
                    putStrLn "Entering debug mode..."
                    ds <- debugMode $ return s
                    putStrLn "Exiting debug mode..."
                    go ds
                "_fix_teleporter" -> go $ fixTeleporter s
                _ -> go $ setStatus (setInput s (l ++ "\n")) Running

debugMode :: IO State -> IO State
debugMode is = do
    putStr "Debug> "
    hFlush stdout
    l <- getLine
    case l of
        "exit" -> is
        "save" -> do
            handleSave is
            debugMode is
        "load" -> do
            a <- handleLoad is
            debugMode $ return a
        "dump" -> do
            handleDump is
            debugMode is
        _ -> do
            putStrLn "I don't understand"
            debugMode is

handleSave :: IO State -> IO ()
handleSave is = do
    putStr "File name to save state into: "
    hFlush stdout
    f <- getLine
    s <- is
    writeFile f (show s)
    return ()

handleLoad :: IO State -> IO State
handleLoad is = do
    putStr "File name to load: "
    hFlush stdout
    f <- getLine
    g <- readFile f
    let ns = read g
    return ns

handleDump :: IO State -> IO ()
handleDump is = let dumpMem :: State -> Int -> [String]
                    dumpMem s p = if p >= (V.length $ mem s)
                                  then []
                                  else let (op, n) = readOp s p
                                       in
                                           (show p ++ ": " ++ show op ++ "\n") :
                                               dumpMem s (p + n)
                in do
                    s <- is
                    writeFile "mem-dump" $ concat $ dumpMem s 0
                    return ()

fixTeleporter :: State -> State
fixTeleporter s = let a = setReg s (Reg 7) 25734
                      b = wmem a 5489 21
                      c = wmem b 5490 21
                  in
                      wmem c 5493 6
