import System.Environment
import GHC.Word
import Data.List
import Text.Printf
import Data.ByteString
import Data.ByteString.Internal (w2c)
import Data.ByteString.Internal (c2w)

main :: IO ()
main = do
    stdIn <- argV
    impl stdIn

argV = do
    f <- getArgs
    case (genericLength f) of
        0 -> Data.ByteString.getContents
        1 -> Data.ByteString.readFile (Data.List.head f)
        _ -> do
            printf "Usage: 'bfi source.b' OR 'cat source.b | bfi'\n" 
            return Data.ByteString.empty

impl :: ByteString -> IO ()
impl str = do
    eval (initial str)
    return ()

getWord = do
    w <- getChar
    return (c2w w)

eval :: BFState -> IO ()
eval state
    | (iptr state) < Data.ByteString.length (inst state)
    =   case (w2c cur) of
            '+' ->  eval (bfInc (istate))
            '-' -> eval (bfDec (istate))
            '>' -> eval (bfIncP (istate))
            '<' -> eval (bfDecP (istate))
            '.' -> do
                putChar (w2c (memhead (rmem state)))
                eval istate
            ',' -> do
                w <- getWord
                eval istate { rmem = w : memtail (rmem state)}
            '[' -> eval (bfBegL state)
            ']' -> eval (bfEndL state)
            '\0' -> do
                print "EOSource! Bad!"
                return ()
            _ -> eval (istate)
    | otherwise = return ()
    where
        cur = curinst state
        istate = incstate state

initial :: ByteString -> BFState
initial str = BFState { lmem = [], rmem = [], iptr = 0, inst = str, lstack = []}


bfInc state =
    state { rmem = (((memhead (rmem state)) + 1) : (memtail (rmem state))) }
    
bfDec state =
    state { rmem = (((memhead (rmem state)) - 1) : (memtail (rmem state))) }
    
bfIncP state =
    state {
        lmem = memhead (rmem state) : (lmem state),
        rmem = memtail (rmem state)
    }
    
bfDecP state =
    state {
        rmem = memhead (lmem state) : (rmem state),
        lmem = memtail (lmem state)
    }

bfOut state = state

bfIn state = state

bfBegL state
    | (memhead (rmem state)) == 0 =
        bfJumpL   state { lstack = (iptr state) : (lstack state), iptr = (iptr state) + 1 } (genericLength (lstack state))
    | otherwise = state { lstack = (iptr state) : (lstack state), iptr = (iptr state) + 1 }
    
bfEndL state
    | (memhead (rmem state)) == 0 =
        state { iptr = (iptr state) + 1, lstack = etail (lstack state)}
    | otherwise = state { iptr = Data.List.head (lstack state), lstack = etail (lstack state) }

bfJumpL state len
    | genericLength (lstack state) - len == 0 = state
    | cur == (c2w '[') = bfJumpL state { lstack = (iptr state) : (lstack state), iptr = (iptr state) + 1 } len
    | cur == (c2w ']') = bfJumpL state { lstack = etail (lstack state), iptr = (iptr state) + 1 } len
    | otherwise = bfJumpL istate len
    where
        cur = curinst state
        istate = incstate state

memhead :: [Word8] -> Word8
memhead [] =  c2w '\0'
memhead (x:_) = x

memtail :: [Word8] -> [Word8]
memtail [] = [0]
memtail (x:[]) = [0]
memtail (_:xs) = xs

etail :: [a] -> [a]
etail [] = []
etail (x:[]) = []
etail (_:xs) = xs
    
curinst state = 
    ((inst state) `Data.ByteString.index` (iptr state))
incstate state=
    (state { iptr = (iptr state) + 1 })
    

data BFState = BFState {
    lmem :: [Word8],
    rmem :: [Word8],
    iptr :: Int,
    inst :: ByteString,
    lstack :: [Int]
}