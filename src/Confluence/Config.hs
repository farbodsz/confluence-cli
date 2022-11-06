--------------------------------------------------------------------------------

-- | Types for the user configuration file and functions for loading it.
module Confluence.Config
    ( Config(..)
    , loadConfig
    ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import           System.Directory               ( XdgDirectory(XdgConfig)
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

-- | Returns the configuration file if it exists.
loadConfig :: IO (Maybe Config)
loadConfig = decode <$> (configFile >>= BL.readFile)

-------------------------------------------------------------------------------
