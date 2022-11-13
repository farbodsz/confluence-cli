--------------------------------------------------------------------------------

module Main where

import qualified Confluence.CLI                as CLI
import           Confluence.Commands
import           Confluence.Config
import qualified Data.Text.IO                  as T
import           Options.Applicative            ( execParser )

--------------------------------------------------------------------------------

main :: IO ()
main = do
    cmd      <- execParser cliArgs
    e_config <- loadConfig

    case e_config of
        Left config_err -> case config_err of
            NoConfigFoundErr path ->
                putStrLn $ "Config file does not exist: " ++ path
            InvalidConfigErr contents ->
                T.putStrLn $ "Invalid config file:\n" <> contents
        Right config -> runCli cmd config

runCli :: CliCommand -> Config -> IO ()
runCli (SpacesCommand SpacesOpts {..}) = CLI.getSpaces optStart optLimit optType

--------------------------------------------------------------------------------
