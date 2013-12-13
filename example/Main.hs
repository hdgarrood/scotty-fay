{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import qualified Data.Text.Lazy as T
import Web.Scotty
import Web.Scotty.Fay

main :: IO ()
main = scotty 3000 $ do
    serveFay $
        -- If the first segment of the request path matches this, try to serve
        -- Fay. Otherwise try the next route.
        under "/scotty-fay" .
        -- Specify the directory where your Fay files are.
        from "src/fay"

    get "/" $ do
        html $
            "<!doctype html>" <>
            "<html>" <>
            "<head>" <>
            "<script type=text/javascript src=/scotty-fay/HelloWorld.hs></script>" <>
            "</head>" <>
            "<body><h1>lol</h1></body>" <>
            "</html>"
