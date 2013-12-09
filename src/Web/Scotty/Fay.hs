module Web.Scotty.Fay (serveFay) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default
import Data.Text.Lazy (Text)
import Web.Scotty.Trans
import Fay

import Web.Scotty.Fay.Utils

serveFay :: MonadIO a => String -> ScottyT Text a ()
serveFay base = do
    get (capture $ base ++ "/:path") $ do
        -- TODO: security: directory traversal
        path <- param "path"
        result <- liftIO (compileFile config path)
        case result of
            Left err        -> raiseCompileErr err
            Right (code, _) -> js code
    where
        raiseCompileErr = raise . stringToLazyText . showCompileError

config :: CompileConfig
config = def

js :: MonadIO a => String -> ActionT Text a ()
js jsString = do
    setHeader "Content-Type" "text/javascript"
    raw $ stringToLazyByteString jsString
