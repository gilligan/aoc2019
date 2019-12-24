{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day5 where

import           Control.Monad.IO.Class
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
  | opcode == 1 = Add (Param (p1 info) v1) (Param (p2 info) v2) (Param (p3 info) v3)
  | opcode == 2 = Mul (Param (p1 info) v1) (Param (p2 info) v2) (Param (p3 info) v3)
  | opcode == 3 = Read (Param (p1 info) v1)
  | opcode == 4 = Print (Param (p1 info) v1)
  | opcode == 99 = Halt
  | otherwise = error $ "invalid opcode: " ++ show info ++ show pc
  where
      info = parseInst $ getM m pc
      opcode = op info
      v1 = getM m (pc + 1)
      v2 = getM m (pc + 2)
      v3 = getM m (pc + 3)

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
        (Add p1 p2 p3) -> runA2 (+) (fetch m p1) (fetch m p2) (dest p3)
        (Mul p1 p2 p3) -> runA2 (*) (fetch m p1) (fetch m p2) (dest p3)
        (Read p1)      -> runRead (dest p1)
        (Print p1)     -> runPrint (fetch m p1)
        Halt           -> runHalt
        where
            dest (Param Position n) = n
            dest (Param Immediate n) = error "dest cannot be in immediate mode"

runProgram :: VM ()
runProgram = do
    step
    (p, m) <- get
    if p > 0
       then runProgram
       else liftIO $ putStrLn "Done"

testCode :: [Int]
testCode = read . T.unpack <$> T.splitOn ","  "3,225,1,225,6,6,1100,1,238,225,104,0,1101,86,8,225,1101,82,69,225,101,36,65,224,1001,224,-106,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,102,52,148,224,101,-1144,224,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,1102,70,45,225,1002,143,48,224,1001,224,-1344,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,1101,69,75,225,1001,18,85,224,1001,224,-154,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1101,15,59,225,1102,67,42,224,101,-2814,224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1101,28,63,225,1101,45,22,225,1101,90,16,225,2,152,92,224,1001,224,-1200,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,1101,45,28,224,1001,224,-73,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1,14,118,224,101,-67,224,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,677,224,102,2,223,223,1005,224,329,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,344,1001,223,1,223,1107,677,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,374,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1007,677,677,224,1002,223,2,223,1005,224,404,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,1108,226,677,224,102,2,223,223,1006,224,434,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1108,226,226,224,1002,223,2,223,1005,224,479,1001,223,1,223,1007,226,677,224,102,2,223,223,1005,224,494,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,509,101,1,223,223,107,677,226,224,1002,223,2,223,1006,224,524,1001,223,1,223,108,677,677,224,102,2,223,223,1006,224,539,101,1,223,223,7,677,226,224,102,2,223,223,1006,224,554,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,569,101,1,223,223,108,677,226,224,1002,223,2,223,1006,224,584,101,1,223,223,108,226,226,224,102,2,223,223,1006,224,599,1001,223,1,223,1107,226,226,224,102,2,223,223,1006,224,614,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,629,1001,223,1,223,107,226,226,224,102,2,223,223,1005,224,644,101,1,223,223,8,226,226,224,102,2,223,223,1006,224,659,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226"
