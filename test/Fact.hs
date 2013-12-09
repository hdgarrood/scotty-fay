module Fact where

-- A near-trivial Fay module for testing scotty-fay
import Prelude

fact :: Int -> Int
fact n = product [1..n]
