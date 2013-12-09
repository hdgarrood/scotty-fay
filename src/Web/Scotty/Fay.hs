module Web.Scotty.Fay  where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Network.Wai (pathInfo, Request)
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
            "scotty-fay: Could not find " ++ file -- TODO: hs relative path?
        else do
            file' <- canonicalizePath file
            res <- Fay.compileFile cfg file'
            case res of
                Right (out, _) -> return $ Success out
                Left err       -> return . Error . Fay.showCompileError $ err

serveFay :: (MonadIO a, Functor a) => T.Text -> ScottyT LT.Text a ()
serveFay base = do
    get (pattern base) $ do
        path <- maybeParam "filePath"
        case path of
            Just p -> do
                result <- liftIO (compileFile config p)
                case result of
                    Success code     -> respondWithJs code
                    Error err        -> raiseErr err
                    FileNotFound msg -> notFoundMsg msg
            Nothing -> notFoundMsg "scotty-fay: requested path is invalid"
    where
        notFoundMsg msg = do
            status notFound404
            text (stringToLazyText msg)

        raiseErr = raise . stringToLazyText

route :: T.Text -> Request -> Maybe [Param]
route base req = do
    base' <- eatFromStart "/" base
    let pathSegments = pathInfo req
    first <- safeHead pathSegments
    rest  <- nonEmptyTail pathSegments

    guard (first == base')
    return . maybeToList . makeParam $ rest
    where
        safeHead            = listToMaybe

        nonEmptyTail []     = Nothing
        nonEmptyTail [_]    = Nothing
        nonEmptyTail (_:xs) = Just xs

pattern :: T.Text -> RoutePattern
pattern = function . route

eatFromStart :: T.Text -> T.Text -> Maybe T.Text
eatFromStart prefix str =
    if prefix `T.isPrefixOf` str
        then Just $ T.drop (T.length prefix) str
        else Nothing

makeParam :: [T.Text] -> Maybe (LT.Text, LT.Text)
makeParam = fmap (\p -> ("filePath", LT.fromStrict p)) . secureRejoin

-- Rejoin path segments to get a file path, while preventing directory
-- traversal attacks.
secureRejoin :: [T.Text] -> Maybe T.Text
secureRejoin xs = do
    guard (noDots xs)
    return $ T.intercalate "/" xs
    where
        noDots = not . elem ".."

maybeParam :: (Functor a, MonadIO a, Parsable b) =>
              LT.Text -> ActionT LT.Text a (Maybe b)
maybeParam key = fmap Just (param key) `rescue` (const $ return Nothing)

config :: Fay.CompileConfig
config = def

respondWithJs :: MonadIO a => String -> ActionT LT.Text a ()
respondWithJs jsString = do
    setHeader "Content-Type" "text/javascript"
    raw $ stringToLazyByteString jsString
