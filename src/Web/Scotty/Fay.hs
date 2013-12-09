module Web.Scotty.Fay (serveFay) where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default
import Data.Maybe (listToMaybe)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Network.Wai (pathInfo)
import Web.Scotty.Trans
import Fay

import Web.Scotty.Fay.Utils

serveFay :: MonadIO a => T.Text -> ScottyT LT.Text a ()
serveFay base = do
    get (makeFayPattern base) $ do
        path <- param "filePath"
        result <- liftIO (compileFile config path)
        case result of
            Left err        -> raiseCompileErr err
            Right (code, _) -> js code
    where
        raiseCompileErr = raise . stringToLazyText . showCompileError

makeFayPattern :: T.Text -> RoutePattern
makeFayPattern base = function $ \req -> do
    let pathSegments = pathInfo req
    first <- safeHead pathSegments
    rest  <- nonEmptyTail pathSegments

    guard (first == base)
    filePath <- secureRejoin rest

    return [("filePath", LT.fromStrict filePath)]
    where
        safeHead            = listToMaybe

        nonEmptyTail []     = Nothing
        nonEmptyTail [_]    = Nothing
        nonEmptyTail (_:xs) = Just xs

secureRejoin :: [T.Text] -> Maybe T.Text
secureRejoin xs = do
    guard (noDots xs)
    return $ T.intercalate "/" xs
    where
        noDots = not . elem ".."

config :: CompileConfig
config = def

js :: MonadIO a => String -> ActionT LT.Text a ()
js jsString = do
    setHeader "Content-Type" "text/javascript"
    raw $ stringToLazyByteString jsString
