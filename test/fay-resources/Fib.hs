module Fib where

import Prelude
import qualified Fib.Internal

fib :: Int -> Int
fib = Fib.Internal.fib
