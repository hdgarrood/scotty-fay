module Web.Scotty.Fay.Internal where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Web.Scotty.Trans
import Network.Wai
import Network.HTTP.Types (notFound404)
import System.Directory

import Web.Scotty.Fay.Config
import Web.Scotty.Fay.Utils


-- General dumping ground for helper functions.

warn :: String -> IO ()
warn = putStrLn . ("scotty-fay: " ++)

-- Run once, at application startup. Used for checking that the configuration
-- is sane.
initialize :: Config -> IO ()
initialize conf = do
    let includeDirs = configIncludeDirs conf
    results <- getNonExistent includeDirs
    warn $ nonExistentWarning results

getNonExistent :: [FilePath] -> IO [FilePath]
getNonExistent = foldl f (return [])
    where
        f acc dir = do
            dirs <- acc
            exists <- doesDirectoryExist dir
            if exists
                then return (dir : dirs)
                else return dirs

nonExistentWarning :: [FilePath] -> String
nonExistentWarning dirs = concat
    [ "The following include dirs:"
    , showAll dirs
    , "do not exist. scotty-fay might not work very well."
    ]
    where showAll = concat . intersperse "\n\t"

-- The route matcher function, to be given to Scotty. Ensure the request path
-- starts with the given base path, and then put the rest of the path into
-- the "filePath" parameter (if it exists, and is valid).
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
makeParam = fmap (\p -> ("path", LT.fromStrict p)) . secureRejoin

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

respondWithJs :: MonadIO a => String -> ActionT LT.Text a ()
respondWithJs jsString = do
    setHeader "Content-Type" "text/javascript"
    raw $ stringToLazyByteString jsString

notFoundMsg :: MonadIO a => String -> ActionT LT.Text a ()
notFoundMsg msg = do
    status notFound404
    text (stringToLazyText msg)

raiseErr :: MonadIO a => String -> ActionT LT.Text a ()
raiseErr = raise . wrapInPreTag . stringToLazyText
    where
        wrapInPreTag str = "<pre>" `LT.append` str `LT.append` "</pre>"
