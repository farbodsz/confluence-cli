--------------------------------------------------------------------------------

module Main (main) where

import Confluence.CLI qualified as CLI
import Confluence.Commands
import Confluence.Config
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative (execParser)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    cmd <- execParser cliArgs
    e_config <- loadConfig

    case e_config of
        Left err -> T.putStrLn $ configErrMsg err
        Right cfg -> case cmd of
            ContentCreateCommand opts ->
                CLI.createContent cfg opts.space opts.title opts.contentType opts.body
            SpacesCommand opts ->
                CLI.getSpaces cfg opts.start opts.limit opts.spaceType

configErrMsg :: ConfigLoadError -> T.Text
configErrMsg (NoConfigFoundErr path) = 
    "Config file does not exist: " <> T.pack path
configErrMsg (InvalidConfigErr contents) = "Invalid config file:\n" <> contents

--------------------------------------------------------------------------------
