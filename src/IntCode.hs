{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IntCode where

import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Control.Monad.State
import           Control.Monad.State.Class
import qualified Data.Text as T

setAt :: [a] -> Int -> a -> [a]
setAt xs i e = case splitAt i xs of
    (old, _:new) -> old ++ e : new
    _ -> xs

class Memory m where
    getM :: m a -> Int -> a
    setM :: m a -> Int -> a -> m a

newtype MemList a = MemList [a]
    deriving (Eq, Foldable, Functor)

instance Memory MemList where
    getM (MemList xs) n   = xs !! n
    setM (MemList xs) n x = MemList $ setAt xs n x

type VmState = (PC, MemList Int)

newtype VmT m a = VM { runVM :: StateT VmState m a }
    deriving (Functor, Applicative, Monad, MonadState VmState, MonadIO)

type VM = VmT IO
type PC = Int
type Addr = Int

data Mode = Immediate | Position
    deriving (Show, Eq)

data Param = Param Mode Int
    deriving (Show, Eq)

data InstInfo = InstInfo { op :: Int
                         , p1 :: Mode
                         , p2 :: Mode
                         , p3 :: Mode
                         } deriving (Show, Eq)

data Inst = Add Param Param Param
          | Mul Param Param Param
          | Read Param
          | Print Param
          | JumpTrue Param Param
          | JumpFalse Param Param
          | LessThan Param Param Param
          | Equals Param Param Param
          | Halt
          deriving (Show, Eq)

showInst :: Int -> String
showInst n = pad 5 $ show n
    where
        pad n s = if length s < 5 then take (5 - length s) "00000" ++ s else s

parseInst :: Int -> InstInfo
parseInst (showInst -> s@(a:b:c:d:e:_)) = InstInfo opcode (toMode c) (toMode b) (toMode a)
    where
        opcode = read $ d : [e]
        toMode '0' = Position
        toMode '1' = Immediate

decode :: Memory m => (PC, m Int) -> Inst
decode (pc, m)
  | opcode == 1 = Add param1 param2 param3
  | opcode == 2 = Mul param1 param2 param3
  | opcode == 3 = Read param1
  | opcode == 4 = Print param1
  | opcode == 5 = JumpTrue param1 param2
  | opcode == 6 = JumpFalse param1 param2
  | opcode == 7 = LessThan param1 param2 param3
  | opcode == 8 = Equals param1 param2 param3
  | opcode == 99 = Halt
  | otherwise = error $ "invalid opcode: " ++ show opMeta ++ show pc
  where
      opMeta = parseInst $ getM m pc
      opcode = op opMeta
      val1 = getM m (pc + 1)
      val2 = getM m (pc + 2)
      val3 = getM m (pc + 3)
      param1 = Param (p1 opMeta) val1
      param2 = Param (p2 opMeta) val2
      param3 = Param (p3 opMeta) val3

fetch :: Memory m => m Int -> Param -> Int
fetch _ (Param Immediate n) = n
fetch m (Param Position n) = getM m n

type F2 = (Int -> Int -> Int)

runA2 :: F2 -> Int -> Int -> Int -> VM ()
runA2 f p1 p2 dest = modify (\(pc, m) -> (pc + 4, setM m dest (f p1 p2)))

runRead :: Int -> VM ()
runRead dest = do
    (pc, m) <- get
    liftIO $ putStr "Enter value :"
    x <- liftIO (read <$> getLine)
    put (pc + 2, setM m dest x)

runPrint :: Int -> VM ()
runPrint p = do
    (pc, m) <- get
    liftIO $ putStrLn $ "print " ++ show p
    put (pc + 2, m)

runJumpTrue :: Int -> Int -> VM ()
runJumpTrue p1 p2 = do
    (pc, m) <- get
    put (if p1 /= 0 then p2 else pc + 3, m)

runJumpFalse :: Int -> Int -> VM ()
runJumpFalse p1 p2 = do
    (pc, m) <- get
    put (if p1 == 0 then p2 else pc + 3, m)

runLessThan :: Int -> Int -> Int -> VM ()
runLessThan p1 p2 dest = do
    (pc, m) <- get
    let x = if p1 < p2 then 1 else 0
    put (pc + 4, setM m dest x)

runEquals :: Int -> Int -> Int -> VM ()
runEquals p1 p2 dest = do
    (pc, m) <- get
    let x = if p1 == p2 then 1 else 0
    put (pc + 4, setM m dest x)

runHalt :: VM ()
runHalt = do
    (pc, m) <- get
    liftIO $ putStrLn "HALT"
    put (-1, m)

step ::VM ()
step = do
    (pc, m) <- get
    let inst = decode (pc, m)
    case inst of
        (Add p1 p2 p3)      -> runA2 (+) (fetch m p1) (fetch m p2) (dest p3)
        (Mul p1 p2 p3)      -> runA2 (*) (fetch m p1) (fetch m p2) (dest p3)
        (Read p1)           -> runRead (dest p1)
        (Print p1)          -> runPrint (fetch m p1)
        (JumpTrue p1 p2)    -> runJumpTrue (fetch m p1) (fetch m p2)
        (JumpFalse p1 p2)   -> runJumpFalse (fetch m p1) (fetch m p2)
        (LessThan p1 p2 p3) -> runLessThan (fetch m p1) (fetch m p2) (dest p3)
        (Equals p1 p2 p3)   -> runEquals (fetch m p1) (fetch m p2) (dest p3)
        Halt                -> runHalt
        where
            dest (Param Position n) = n
            dest (Param Immediate n) = error "dest cannot be in immediate mode"


runIntCode :: [Int] -> IO ()
runIntCode prog = void $ runStateT (runVM runProg) (0, MemList prog)
    where
        runProg :: VmT IO ()
        runProg = whileM_ isRunning step

        isRunning :: VM Bool
        isRunning = get >>= \(p, m) -> return $ p > 0
