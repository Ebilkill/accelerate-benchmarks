{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import DotProduct (dotpBenches)

import Criterion.Main
import Criterion.Types

config :: Config
config = defaultConfig {
    timeLimit = 2.0
  , reportFile = Just $ "accelerate-benchmarks.html"
  }

main :: IO ()
main = 
  do
    dotp <- dotpBenches
    defaultMainWith config [
        dotp
      ]

