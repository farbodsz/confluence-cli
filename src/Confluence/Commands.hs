--------------------------------------------------------------------------------

module Confluence.Commands
    ( cliArgs
    , CliCommand(..)
    ) where

import           Data.Version                   ( showVersion )
import           Options.Applicative
import           Paths_confluence_cli           ( version )

--------------------------------------------------------------------------------

data CliCommand = SpacesCommand
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
commandP =
    hsubparser $ command "spaces" (info spacesCmdP $ progDesc "Get spaces")

spacesCmdP :: Parser CliCommand
spacesCmdP = pure SpacesCommand

--------------------------------------------------------------------------------
