module Web.Scotty.Fay.Utils where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC8
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text.Encoding as TE

stringToLazyByteString :: String -> LB.ByteString
stringToLazyByteString = LBC8.pack

stringToLazyText :: String -> LT.Text
stringToLazyText = LTE.decodeUtf8 . stringToLazyByteString

strictByteStringToLazyText :: B.ByteString -> LT.Text
strictByteStringToLazyText = LT.fromStrict . TE.decodeUtf8
