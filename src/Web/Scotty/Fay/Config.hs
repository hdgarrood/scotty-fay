module Web.Scotty.Fay.Config
    ( Config
    , ConfigBuilder
    , configSrcDir
    , configBasePath
    , buildConfig
    , under
    , from
    , toFay
    ) where

import Data.Default
import qualified Data.Text as T
import qualified Fay
import qualified Fay.Compiler.Config as Fay

data Config = Config
    { configBasePath :: T.Text
    , configSrcDir   :: FilePath
    }

instance Default Config where
    def = Config
        { configBasePath = ""
        , configSrcDir   = ""
        }

type ConfigBuilder = Config -> Config

buildConfig :: ConfigBuilder -> Config
buildConfig f = f def

-- Convert a scotty-fay Config to a Fay CompileConfig
toFay :: Config -> Fay.CompileConfig
toFay conf = Fay.addConfigDirectoryIncludePaths [configSrcDir conf] $ def

under :: T.Text -> ConfigBuilder
under basePath conf = conf { configBasePath = basePath }

from :: FilePath -> ConfigBuilder
from dir conf = conf { configSrcDir = dir }
