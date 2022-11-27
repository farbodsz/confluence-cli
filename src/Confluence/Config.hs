--------------------------------------------------------------------------------

-- | Types for the user configuration file and functions for loading it.
module Confluence.Config (
    Config (..),
    loadConfig,
    ConfigLoadError (..),
) where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Either.Extra (maybeToEither)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import GHC.Generics (Generic)
import System.Directory (
    XdgDirectory (XdgConfig),
    doesFileExist,
    getXdgDirectory,
 )
import System.FilePath ((</>))

--------------------------------------------------------------------------------

data Config = Config
    { email :: T.Text
    , token :: T.Text
    , url :: T.Text
    }
    deriving (Generic, Show)

instance FromJSON Config
instance ToJSON Config

-------------------------------------------------------------------------------

-- | Path to the configuration file.
configFile :: IO FilePath
configFile = do
    configDir <- getXdgDirectory XdgConfig "confluence-cli"
    pure $ configDir </> "config.json"

-------------------------------------------------------------------------------

data ConfigLoadError
    = -- | Couldn't find config file: path searched for config file
      NoConfigFoundErr FilePath
    | -- | Error in parsing to JSON format: file contents
      InvalidConfigErr T.Text
    deriving (Show)

-- | Returns the configuration file, or an error if unable to load.
loadConfig :: IO (Either ConfigLoadError Config)
loadConfig = do
    fpath <- configFile
    exists <- doesFileExist fpath
    if not exists
        then pure $ Left (NoConfigFoundErr fpath)
        else do
            contents <- configFile >>= BL.readFile
            let m_config = decode contents
            pure $
                maybeToEither
                    (InvalidConfigErr . TL.toStrict . TLE.decodeUtf8 $ contents)
                    m_config

-------------------------------------------------------------------------------
