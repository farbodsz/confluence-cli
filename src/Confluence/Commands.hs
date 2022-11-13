--------------------------------------------------------------------------------

module Confluence.Commands
    ( cliArgs
    , CliCommand(..)
    , SpacesOpts(..)
    ) where

import           Confluence.Types
import           Data.String                    ( IsString )
import           Data.Version                   ( showVersion )
import           Options.Applicative
import           Paths_confluence_cli           ( version )

--------------------------------------------------------------------------------

data CliCommand = SpacesCommand SpacesOpts
    deriving Eq

--------------------------------------------------------------------------------

cliArgs :: ParserInfo CliCommand
cliArgs = info (commandP <**> helper) (cliHeader <> fullDesc)

cliHeader :: InfoMod CliCommand
cliHeader = header $ "Confluence CLI v" ++ cliVersion

cliVersion :: String
cliVersion = showVersion version

--------------------------------------------------------------------------------
-- Spaces

data SpacesOpts = SpacesOpts
    { optStart :: Int
    , optLimit :: Int
    , optType  :: Maybe SpaceType
    }
    deriving Eq

commandP :: Parser CliCommand
commandP =
    hsubparser $ command "spaces" (info spacesCmdP $ progDesc "Get spaces")

spacesCmdP :: Parser CliCommand
spacesCmdP =
    fmap SpacesCommand
        $   SpacesOpts
        <$> startOptP
        <*> limitOptP
        <*> spaceTypeOptP

spaceTypeOptP :: Parser (Maybe SpaceType)
spaceTypeOptP = optional $ option
    typeReader
    (  long "type"
    <> help "Filter results to spaces based on their type"
    <> metavar "global|personal"
    )

--------------------------------------------------------------------------------
-- Common options

startOptP :: Parser Int
startOptP = option
    auto
    (  long "start"
    <> help "Starting index of the returned spaces"
    <> showDefault
    <> value 0
    <> metavar "INT"
    )

limitOptP :: Parser Int
limitOptP = option
    auto
    (  long "limit"
    <> help "Maximum number of spaces to return per page"
    <> showDefault
    <> value 25
    <> metavar "INT"
    )

--------------------------------------------------------------------------------
-- Option parsing for custom types

class ReadOpt a where
    readOpt :: (Eq s, IsString s) => s ->  Maybe a

typeReader :: ReadOpt a => ReadM a
typeReader = maybeReader readOpt

instance ReadOpt SpaceType where
    readOpt "global"   = Just GlobalSpace
    readOpt "personal" = Just PersonalSpace
    readOpt _          = Nothing

--------------------------------------------------------------------------------
