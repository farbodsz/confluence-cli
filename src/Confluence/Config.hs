--------------------------------------------------------------------------------

-- | Types for the user configuration file and functions for loading it.
module Confluence.Config
    ( Config(..)
    , loadConfig
    , ConfigLoadError(..)
    ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy          as BL
import           Data.Either.Extra              ( maybeToEither )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TLE
import           System.Directory               ( XdgDirectory(XdgConfig)
                                                , doesFileExist
                                                , getXdgDirectory
                                                )
import           System.FilePath                ( (</>) )

--------------------------------------------------------------------------------

data Config = Config
    { cfgEmail    :: T.Text
    , cfgApiToken :: T.Text
    , cfgUrl      :: T.Text
    }
    deriving Show

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v ->
        Config <$> (v .: "email") <*> (v .: "token") <*> (v .: "url")

instance ToJSON Config where
    toJSON Config {..} =
        object ["email" .= cfgEmail, "token" .= cfgApiToken, "url" .= cfgUrl]

-------------------------------------------------------------------------------

-- | Path to the configuration file.
configFile :: IO FilePath
configFile = do
    configDir <- getXdgDirectory XdgConfig "confluence-cli"
    pure $ configDir </> "config.json"

-------------------------------------------------------------------------------

data ConfigLoadError
    = NoConfigFoundErr FilePath
    -- ^ Couldn't find config file: path searched for config file
    | InvalidConfigErr T.Text
    -- ^ Error in parsing to JSON format: file contents
    deriving Show

-- | Returns the configuration file, or an error if unable to load.
loadConfig :: IO (Either ConfigLoadError Config)
loadConfig = do
    fpath  <- configFile
    exists <- doesFileExist fpath
    if not exists
        then pure $ Left (NoConfigFoundErr fpath)
        else do
            contents <- configFile >>= BL.readFile
            let m_config = decode contents
            pure $ maybeToEither
                (InvalidConfigErr . TL.toStrict . TLE.decodeUtf8 $ contents)
                m_config

-------------------------------------------------------------------------------
