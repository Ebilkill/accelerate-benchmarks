{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module DotProduct
  ( dotpBenches
  )
where

import Prelude as P

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU

import Criterion.Main
import Control.DeepSeq

import Foreign.C.Types (CSize(..))
import Foreign.ForeignPtr
import Foreign.Ptr

program :: Acc (Array (Z :. Int) Float) -> Acc (Array (Z :. Int) Float) -> Acc (Array Z Float)
program xs ys = A.fold (+) 0 $ A.zipWith (*) xs ys

-- Use env function?
-- But we don't really use IO here...
aBench :: Native -> (String, Int) -> IO Benchmark
aBench target (name, count) =
  do
    let !p = force $ CPU.runNWith target program
    -- let !p = force $ CPU.runN program
    let !xs = force xs'
    let !ys = force ys'
    return $ bench name $ whnf (p xs) ys
  where
    xs', ys' :: Array (Z :. Int) Float
    xs' = fromList (Z :. count) [0..]
    ys' = fromList (Z :. count) [1, 3..]

newtype FPtr a = FPtr (ForeignPtr a)
instance NFData a => NFData (FPtr a) where
  rnf (FPtr x) = seq x ()

-- Originally, this benchmark used `newArray (take len' [0..])`. However, the
-- time the initialization took was horrible using that. Therefore, the
-- newArray calls have been replaced by mallocArray calls and calls to C
-- functions that generate the same data originally present in the benchmarks.
-- However, both methods led to data leaks; this has been fixed by using
-- foreign pointers. The performance difference between foreign pointers and
-- basic pointers during the benchmarks is very low, so it should not cause any
-- issues for the benchmarks.
cBench :: (String, Int) -> Benchmark
cBench (name, len') =
  let !len = P.fromIntegral len'
      e = do
          xs <- mallocForeignPtrArray len'
          ys <- mallocForeignPtrArray len'
          withForeignPtr xs (initArray1 len)
          withForeignPtr ys (initArray2 len)
          return (FPtr xs, FPtr ys)
      f ~(FPtr xs, FPtr ys) =
          bench name $
          whnfIO $
          withForeignPtr xs $ \xs' ->
          withForeignPtr ys $ \ys' ->
          dot xs' ys' len
  in  env e f

aBenches :: [(String, Int)] -> [Int] -> IO [Benchmark]
aBenches xs = mapM fn
  where
    fn n =
      do
        t <- createTarget [0..n - 1]
        bgroup (show n P.++ "-threads") <$> traverse (aBench t) xs

cBenches :: [(String, Int)] -> [Benchmark]
cBenches = fmap cBench

dotpBenches :: IO Benchmark
dotpBenches =
  do
    let bsc = cBenches runs
    bsa <- aBenches runs [1, 2, 4, 8, 16, 32]
    return $ bgroup "dotp" [
          bgroup "Hand-written" bsc
        , bgroup "Accelerate"   bsa
      ]
  where
    runs = [
        -- Deactivate the tiniest benchmarks for now
        {-("1 float",     1)
      ,   ("1K floats",   1000)
      , -}("1M floats",   1000000)
      , ("100M floats", 100000000)
      ]


foreign import ccall unsafe "dot" dot :: Ptr Float -> Ptr Float -> CSize -> IO Float
foreign import ccall unsafe "init_array1" initArray1 :: CSize -> Ptr Float -> IO ()
foreign import ccall unsafe "init_array2" initArray2 :: CSize -> Ptr Float -> IO ()

