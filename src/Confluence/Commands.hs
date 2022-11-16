--------------------------------------------------------------------------------

module Confluence.Commands (
    cliArgs,
    CliCommand (..),
    ContentCreateOpts (..),
    SpacesOpts (..),
) where

import Confluence.Types
import Data.String (IsString)
import Data.Text qualified as T
import Data.Version (showVersion)
import Options.Applicative
import Paths_confluence_cli (version)

--------------------------------------------------------------------------------
-- Root

data CliCommand
    = ContentCreateCommand ContentCreateOpts
    | SpacesCommand SpacesOpts
    deriving (Eq)

cliArgs :: ParserInfo CliCommand
cliArgs = info (commandP <**> helper) (cliHeader <> fullDesc)

cliHeader :: InfoMod CliCommand
cliHeader = header $ "Confluence CLI v" ++ cliVersion

cliVersion :: String
cliVersion = showVersion version

commandP :: Parser CliCommand
commandP =
    hsubparser $
        command "content" (info contentCreateCmdP $ progDesc "Create content")
            <> command "spaces" (info spacesCmdP $ progDesc "Get spaces")

--------------------------------------------------------------------------------
-- Content

-- TODO: status, representation
data ContentCreateOpts = ContentCreateOpts
    { space :: SpaceKey
    , title :: T.Text
    , contentType :: ContentType
    , filePath :: FilePath
    }
    deriving (Eq)

contentCreateCmdP :: Parser CliCommand
contentCreateCmdP =
    fmap ContentCreateCommand $
        ContentCreateOpts
            <$> strOption
                ( long "space"
                    <> help "Space that the content is being created in"
                    <> metavar "STRING"
                )
            <*> strOption (long "title" <> metavar "STRING")
            <*> contentTypeOptP
            <*> strOption (long "path" <> metavar "FILEPATH")

contentTypeOptP :: Parser ContentType
contentTypeOptP =
    option
        typeReader
        ( long "type"
            <> help "Type of the new content, e.g. \"page\""
            <> metavar "STRING"
            <> showDefault
            <> value PageContent
        )

--------------------------------------------------------------------------------
-- Spaces

data SpacesOpts = SpacesOpts
    { start :: Int
    , limit :: Int
    , spaceType :: Maybe SpaceType
    }
    deriving (Eq)

spacesCmdP :: Parser CliCommand
spacesCmdP =
    fmap SpacesCommand $
        SpacesOpts
            <$> startOptP
            <*> limitOptP
            <*> spaceTypeOptP

spaceTypeOptP :: Parser (Maybe SpaceType)
spaceTypeOptP =
    optional $
        option
            typeReader
            ( long "type"
                <> help "Filter results to spaces based on their type"
                <> metavar "global|personal"
            )

--------------------------------------------------------------------------------
-- Common options

startOptP :: Parser Int
startOptP =
    option
        auto
        ( long "start"
            <> help "Starting index of the returned spaces"
            <> showDefault
            <> value 0
            <> metavar "INT"
        )

limitOptP :: Parser Int
limitOptP =
    option
        auto
        ( long "limit"
            <> help "Maximum number of spaces to return per page"
            <> showDefault
            <> value 25
            <> metavar "INT"
        )

--------------------------------------------------------------------------------
-- Option parsing for custom types

class ReadOpt a where
    readOpt :: (Eq s, IsString s) => s -> Maybe a

typeReader :: ReadOpt a => ReadM a
typeReader = maybeReader readOpt

instance ReadOpt ContentType where
    readOpt "page" = Just PageContent
    readOpt "blogpost" = Just BlogpostContent
    readOpt "attachment" = Just AttachmentContent
    readOpt "content" = Just ContentContent
    readOpt _ = Nothing

instance ReadOpt SpaceType where
    readOpt "global" = Just GlobalSpace
    readOpt "personal" = Just PersonalSpace
    readOpt _ = Nothing

--------------------------------------------------------------------------------
