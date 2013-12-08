module Web.Scotty.FayServer (serveFay) where

import Control.Monad.IO.Class (liftIO)
import Data.Default
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC8
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text.Encoding as TE
import Network.Wai
import Web.Scotty
import Fay

config :: CompileConfig
config = def

stringToLazyByteString :: String -> LB.ByteString
stringToLazyByteString = LBC8.pack

stringToLazyText :: String -> LT.Text
stringToLazyText = LTE.decodeUtf8 . stringToLazyByteString

strictByteStringToLazyText :: B.ByteString -> LT.Text
strictByteStringToLazyText = LT.fromStrict . TE.decodeUtf8

pattern :: RoutePattern
pattern = function $
    \req -> Just [("path", strictByteStringToLazyText $ rawPathInfo req)]

serveFay :: ScottyM ()
serveFay = do
    get pattern $ do
        -- TODO: security: directory traversal
        path <- param "path"
        result <- liftIO (compileFile config path)
        case result of
            Left err        -> raise $ stringToLazyText $ showCompileError err
            Right (code, _) -> js code

js :: String -> ActionM ()
js jsString = do
    setHeader "Content-Type" "text/javascript"
    raw $ LBC8.pack jsString
