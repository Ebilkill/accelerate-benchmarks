{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TupleSections #-}
module Main where

import DotProduct (dotpBenches)
import RadixSort

import Data.Array.Accelerate.LLVM.Native as CPU

import Criterion.Main
import Criterion.Types

config :: Config
config = defaultConfig {
    timeLimit = 10.0
  , reportFile = Just $ "accelerate-benchmarks.html"
  }

makeThreadGroups :: [Int] -> IO [(Native, Int)]
makeThreadGroups xss = mapM f xss
  where
    f x = do
      let xs = [0..x-1]
      t <- createTarget xs
      return (t, x)

main :: IO ()
main = 
  do
    threads <- makeThreadGroups [1, 2, 4, 8, 16, 32]
    dotp <- dotpBenches threads
    radixSort <- radixSortBenches threads
    defaultMainWith config
      [ dotp
      , radixSort
      ]

