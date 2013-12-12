module HelloWorld where

import Prelude
import FFI

alert :: String -> Fay ()
alert = ffi "alert(%1)"

main :: Fay ()
main = alert "hello, world"
