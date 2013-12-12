module Web.Scotty.Fay
    ( module Web.Scotty.Fay.Config
    , serveFay
    , serveFay'
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Lazy as LT
import Web.Scotty.Trans hiding (file)
import qualified Fay
import System.Directory
import System.FilePath

import Web.Scotty.Fay.Internal
import Web.Scotty.Fay.Config

data CompileResult = Success String
                   | Error String
                   | FileNotFound String

compileFile :: Config -> FilePath -> IO CompileResult
compileFile conf fullPath = do
    let (dir, file) = splitFileName fullPath
    let includeDirs = map (dir </>) $ configIncludeDirs conf
    file' <- findFile includeDirs file
    case file' of
        Nothing     -> return . FileNotFound $
            "scotty-fay: Could not find " ++ file -- TODO: hs relative path?
        Just file'' -> do
            res <- Fay.compileFile (toFay conf) file''
            case res of
                Right (out, _) -> return $ Success out
                Left err       -> return . Error . Fay.showCompileError $ err

serveFay :: (MonadIO a, Functor a) => ConfigBuilder -> ScottyT LT.Text a ()
serveFay = serveFay' . buildConfig

serveFay' :: (MonadIO a, Functor a) => Config -> ScottyT LT.Text a ()
serveFay' conf = do
    liftIO $ initialize conf

    get (pattern $ configBasePath conf) $ do
        path <- maybeParam "path"
        case path of
            Just path' -> do
                result <- liftIO (compileFile conf path')
                case result of
                    Success code     -> respondWithJs code
                    Error err        -> raiseErr err
                    FileNotFound msg -> notFoundMsg msg
            Nothing -> notFoundMsg "scotty-fay: requested path is invalid"
