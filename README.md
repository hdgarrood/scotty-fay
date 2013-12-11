scotty-fay
==========

A Scotty application which compiles and serves [Fay][] on request.

Example:


```haskell
-- src/fay/HelloWorld.hs

import Prelude
import FFI

alert :: String -> Fay ()
alert = ffi "alert(%1)"

main :: Fay ()
main = alert "hello, world"
```

```haskell
-- Main.hs
import Web.Scotty
import qualfied Web.Scotty.Fay as SF

main :: IO ()
main = scotty 3000 $ do
    serveFay $
        -- If the first segment of the request path matches this, try to serve
        -- Fay. Otherwise try the next route.
        under "/scotty-fay" .
        -- Specify the directory where your Fay files are.
        from "src/fay"

    get "/" $ do
        html "<script type=text/javascript src=/scotty-fay/HelloWorld.hs>"
```

[Fay]: http://fay-lang.org
