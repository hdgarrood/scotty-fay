module Web.Scotty.Fay
    ( module Web.Scotty.Fay.Config
    , compileFile
    , serveFay
    , serveFay'
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Lazy as LT
import Web.Scotty.Trans hiding (file)
import qualified Fay
import System.Directory

import Web.Scotty.Fay.Internal
import Web.Scotty.Fay.Config

data CompileResult = Success String
                   | Error String
                   | FileNotFound String

compileFile :: Config -> FilePath -> IO CompileResult
compileFile conf file = do
    exists <- doesFileExist file
    if not exists
        then return . FileNotFound $
            "scotty-fay: Could not find " ++ file -- TODO: hs relative path?
        else do
            file' <- canonicalizePath file
            res <- Fay.compileFile (toFay conf) file'
            case res of
                Right (out, _) -> return $ Success out
                Left err       -> return . Error . Fay.showCompileError $ err

serveFay :: (MonadIO a, Functor a) => ConfigBuilder -> ScottyT LT.Text a ()
serveFay = serveFay' . buildConfig

serveFay' :: (MonadIO a, Functor a) => Config -> ScottyT LT.Text a ()
serveFay' conf = do
    liftIO $ initialize conf

    get (pattern $ configBasePath conf) $ do
        path <- maybeParam "filePath"
        case path of
            Just p -> do
                result <- liftIO (compileFile conf p)
                case result of
                    Success code     -> respondWithJs code
                    Error err        -> raiseErr err
                    FileNotFound msg -> notFoundMsg msg
            Nothing -> notFoundMsg "scotty-fay: requested path is invalid"
    where
