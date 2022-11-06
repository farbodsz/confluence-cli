--------------------------------------------------------------------------------

module Confluence.CLI
    ( cliArgs
    , CliCommand(..)
    ) where

import           Data.Version                   ( showVersion )
import           Options.Applicative
import           Paths_confluence_cli           ( version )

--------------------------------------------------------------------------------

data CliCommand = ApiCommand
    deriving Eq

--------------------------------------------------------------------------------

cliArgs :: ParserInfo CliCommand
cliArgs = info (commandP <**> helper) (cliHeader <> fullDesc)

cliHeader :: InfoMod CliCommand
cliHeader = header $ "Confluence CLI v" ++ cliVersion

cliVersion :: String
cliVersion = showVersion version

--------------------------------------------------------------------------------

commandP :: Parser CliCommand
commandP = hsubparser
    $ command "api" (info apiCmdP $ progDesc "Access the API endpoints")

apiCmdP :: Parser CliCommand
apiCmdP = pure ApiCommand

--------------------------------------------------------------------------------
