{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module RadixSort
  ( radixSortBenches
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

radixSortStep :: Acc (Array DIM1 Int) -> Int -> Acc (Array DIM1 Int)
radixSortStep xs b =
  let
    -- Apparently I can't just say (\x -> (x >> b) & 1)...
    bits = A.map ((`A.mod` 2) . (`A.div` (2 P.^ b))) xs
    bits_neg = A.map (1 A.-) bits
    offs = A.fold (+) 0 bits_neg
    idxs_unset = A.zipWith (*) bits_neg (A.scanl (+) 0 bits_neg)
    idxs_set   = A.zipWith (*) bits (A.map (+ the offs) $ A.scanl (+) 0 bits)
    idxs_all   = A.zipWith (+) idxs_set idxs_unset
    --idxs_final = A.map (\x -> x - 1) idxs_all
    new_xs = generate (shape xs) (const (-100))
    res = scatter idxs_all new_xs xs
  in
    res

radixSort :: Acc (Array DIM1 Int) -> Acc (Array DIM1 Int)
radixSort xs =
  P.foldl radixSortStep xs [0..31] -- [0..31]

-- Use env function?
-- But we don't really use IO here...
aBench :: Native -> (String, Int) -> IO Benchmark
aBench target (name, count) =
  do
    return $ env (return (xs, CPU.runNWith target radixSort)) $ (\ ~(xs, p) -> bench name $ whnf p xs)
  where
    xs :: Array (Z :. Int) Int
    xs = fromList (Z :. count) [0..]

aBenches :: [(String, Int)] -> [(Native, Int)] -> IO [Benchmark]
aBenches xs = mapM fn
  where
    fn (t, n) =
      do
        bgroup (show n P.++ "-threads") <$> traverse (aBench t) xs

newtype FPtr a = FPtr (ForeignPtr a)
instance NFData a => NFData (FPtr a) where
  rnf (FPtr x) = seq x ()

{-
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

cBenches :: [(String, Int)] -> [Benchmark]
cBenches = fmap cBench
-}

radixSortBenches :: [(Native, Int)] -> IO Benchmark
radixSortBenches threads =
  do
    bsa <- aBenches runs threads
    return $ bgroup "radix-sort" [
          bgroup "Accelerate"   bsa
      ]
  where
    runs = [
        -- Deactivate the tiniest benchmarks for now
        {-("1 int",     1)
      ,   ("1K ints",   1000)
      , -}("1M ints",   1000000)
      --, ("100M ints", 100000000)
      ]

