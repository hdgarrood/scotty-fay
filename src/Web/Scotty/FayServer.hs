module Web.Scotty.FayServer (serveFay) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default
import Data.Text.Lazy (Text)
import Network.Wai
import Web.Scotty.Trans
import Fay

import Web.Scotty.FayServer.Utils

config :: CompileConfig
config = def

pattern :: RoutePattern
pattern = function $
    \req -> Just [("path", strictByteStringToLazyText $ rawPathInfo req)]

serveFay :: MonadIO a => ScottyT Text a ()
serveFay = do
    get pattern $ do
        -- TODO: security: directory traversal
        path <- param "path"
        result <- liftIO (compileFile config path)
        case result of
            Left err        -> raiseCompileErr err
            Right (code, _) -> js code
    where
        raiseCompileErr = raise . stringToLazyText . showCompileError

js :: MonadIO a => String -> ActionT Text a ()
js jsString = do
    setHeader "Content-Type" "text/javascript"
    raw $ stringToLazyByteString jsString
