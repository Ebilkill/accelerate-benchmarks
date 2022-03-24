{-# LANGUAGE TypeFamilies #-}
module Common
  ( {-Inputs(..)
  , runAccelerateBenchmark -}
  )
where

{-
 - I can dream, okay :(
 - This is a thing I tried to make for a general "hey run this benchmark with
 - these arguments for me" but that was much harder than I thought it'd be, and
 - then I gave up for now
import Data.Array.Accelerate as A

infixr 9 :->
data Inputs sh a
  = Result
  | Array sh a :-> Inputs sh a

type family AF sh a where
  AF sh (Acc (Array sh a) -> Acc (Array sh a)) = AF sh Int

runAccelerateBenchmark :: Native -> String -> Inputs sh a -> AF sh a -> IO Benchmark
runAccelerateBenchmark target name inputs = undefined
-}

