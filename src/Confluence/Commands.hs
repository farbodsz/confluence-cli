--------------------------------------------------------------------------------

module Confluence.Commands (
    cliArgs,
    ConfluenceCmd (..),
    ContentCreateOpts (..),
    SpacesListOpts (..),
) where

import Confluence.Types
import Data.String (IsString)
import Data.Text qualified as T
import Data.Version (showVersion)
import Options.Applicative
import Paths_confluence_cli (version)

--------------------------------------------------------------------------------
-- Root

data ConfluenceCmd
    = ContentCreateCommand ContentCreateOpts
    | SpacesListCommand SpacesListOpts
    deriving (Eq)

cliArgs :: ParserInfo ConfluenceCmd
cliArgs = info (confluenceP <**> helper) (cliHeader <> fullDesc)

cliHeader :: InfoMod ConfluenceCmd
cliHeader = header $ "Confluence CLI v" ++ cliVersion

cliVersion :: String
cliVersion = showVersion version

confluenceP :: Parser ConfluenceCmd
confluenceP =
    hsubparser $
        command "content" (info contentP $ progDesc "Content")
            <> command "spaces" (info spacesP $ progDesc "Spaces")

--------------------------------------------------------------------------------
-- Content

contentP :: Parser ConfluenceCmd
contentP =
    hsubparser $
        command "add" (info contentCreateP $ progDesc "Add content")

-- TODO: status, representation
data ContentCreateOpts = ContentCreateOpts
    { space :: SpaceKey
    , title :: T.Text
    , contentType :: ContentType
    , filePath :: FilePath
    }
    deriving (Eq)

contentCreateP :: Parser ConfluenceCmd
contentCreateP =
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

spacesP :: Parser ConfluenceCmd
spacesP =
    hsubparser $
        command "list" (info spacesListP $ progDesc "List spaces")

data SpacesListOpts = SpacesListOpts
    { start :: Int
    , limit :: Int
    , spaceType :: Maybe SpaceType
    }
    deriving (Eq)

spacesListP :: Parser ConfluenceCmd
spacesListP =
    fmap SpacesListCommand $
        SpacesListOpts
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
