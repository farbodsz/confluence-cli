--------------------------------------------------------------------------------

module Confluence.CLI  where

import Control.Applicative ((<**>))
import Data.Version (showVersion)
import Options.Applicative
    ( InfoMod
    , Parser
    , ParserInfo
    , command
    , fullDesc
    , header
    , helper
    , hsubparser
    , info
    , progDesc
    )
import Paths_confluence_cli (version)

--------------------------------------------------------------------------------

data CliCommand = CmdApi
    deriving (Eq)

cliArgs :: ParserInfo CliCommand
cliArgs = info (pCommand <**> helper) (cliHeader <> fullDesc)

cliHeader :: InfoMod CliCommand
cliHeader = header $ "Confluence CLI v" ++ cliVersion

cliVersion :: String
cliVersion = showVersion version

pCommand :: Parser CliCommand
pCommand =
    hsubparser $
        command "api" (info pCmdApi $ progDesc "Access the API endpoints")

pCmdApi :: Parser CliCommand
pCmdApi = pure CmdApi

--------------------------------------------------------------------------------
