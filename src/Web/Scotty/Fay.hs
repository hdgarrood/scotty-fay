module Web.Scotty.Fay (serveFay) where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default
import Data.Maybe (listToMaybe)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Network.Wai (pathInfo)
import Network.HTTP.Types (notFound404)
import Web.Scotty.Trans hiding (file)
import qualified Fay
import System.Directory

import Web.Scotty.Fay.Utils

data CompileResult = Success String | Error String | FileNotFound String

compileFile :: Fay.CompileConfig -> FilePath -> IO CompileResult
compileFile cfg file = do
    exists <- doesFileExist file
    if not exists
        then return . FileNotFound $
            "scotty-fay: Could not find " ++ file -- TODO: hs relative path
        else do
            file' <- canonicalizePath file
            res   <- Fay.compileFile cfg file'
            case res of
                Right (out, _) -> return $ Success out
                Left err       -> return . Error . Fay.showCompileError $ err

serveFay :: MonadIO a => T.Text -> ScottyT LT.Text a ()
serveFay base = do
    get (makeFayPattern base) $ do
        path <- param "filePath"
        result <- liftIO (compileFile config path)
        case result of
            Success code     -> respondWithJs code
            Error err        -> raiseErr err
            FileNotFound msg -> notFoundMsg msg
    where
        notFoundMsg msg = do
            status notFound404
            text (stringToLazyText msg)

        raiseErr = raise . stringToLazyText

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

-- Rejoin path segments to get a file path, while preventing directory
-- traversal attacks.
secureRejoin :: [T.Text] -> Maybe T.Text
secureRejoin xs = do
    guard (noDots xs)
    return $ T.intercalate "/" xs
    where
        noDots = not . elem ".."

config :: Fay.CompileConfig
config = def

respondWithJs :: MonadIO a => String -> ActionT LT.Text a ()
respondWithJs jsString = do
    setHeader "Content-Type" "text/javascript"
    raw $ stringToLazyByteString jsString
