{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import DotProduct (dotpBenches)

import Criterion.Main

main :: IO ()
main = 
  do
    dotp <- dotpBenches
    defaultMain [
        dotp
      ]

