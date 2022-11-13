--------------------------------------------------------------------------------

module Main where

import qualified Confluence.CLI as CLI
import Confluence.Commands
import Confluence.Config
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative (execParser)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    cmd <- execParser cliArgs
    e_config <- loadConfig

    case e_config of
        Left err -> T.putStrLn $ configErrMsg err
        Right cfg -> case cmd of
            SpacesCommand (SpacesOpts {..}) ->
                CLI.getSpaces cfg optStart optLimit optType

configErrMsg :: ConfigLoadError -> T.Text
configErrMsg (NoConfigFoundErr path) = "Config file does not exist: " <> T.pack path
configErrMsg (InvalidConfigErr contents) = "Invalid config file:\n" <> contents

--------------------------------------------------------------------------------
