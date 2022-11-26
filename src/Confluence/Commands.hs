--------------------------------------------------------------------------------

module Confluence.Commands (
    cliArgs,
    ConfluenceCmd (..),
    ContentCreateOpts (..),
    ContentListOpts (..),
    SpacesListOpts (..),
) where

import Confluence.TextConversions (FromText (fromText))
import Confluence.Types
import Data.Text qualified as T
import Data.Version (showVersion)
import Options.Applicative
import Paths_confluence_cli (version)

--------------------------------------------------------------------------------
-- Root

data ConfluenceCmd
    = ContentCreateCommand ContentCreateOpts
    | ContentListCommand ContentListOpts
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
            <> command "list" (info contentListP $ progDesc "List content")

data ContentCreateOpts = ContentCreateOpts
    { space :: SpaceKey
    , title :: T.Text
    , filePath :: FilePath
    , contentType :: ContentType
    , status :: ContentStatus
    , representation :: ContentRepresentation
    }
    deriving (Eq)

contentCreateP :: Parser ConfluenceCmd
contentCreateP =
    fmap ContentCreateCommand $
        ContentCreateOpts
            <$> optSpaceKeyP
            <*> optContentTitleP
            <*> strOption (long "path" <> metavar "FILEPATH")
            <*> optContentTypeP
            <*> optContentStatusP
            <*> optContentRepresentationP

optSpaceKeyP :: Parser SpaceKey
optSpaceKeyP =
    strOption
        ( long "space"
            <> help "Space that the content belongs to"
            <> metavar "STRING"
        )

optContentTitleP :: Parser T.Text
optContentTitleP =
    strOption
        ( long "title"
            <> help "Content title"
            <> metavar "STRING"
        )

-- | Parser for an optional 'ContentType' option, with a default value of
-- 'PageContent'.
optContentTypeP :: Parser ContentType
optContentTypeP =
    option
        typeReader
        ( long "type"
            <> help "Type of the new content, e.g. \"page\""
            <> metavar "STRING"
            <> showDefault
            <> value PageContent
        )

optContentStatusP :: Parser ContentStatus
optContentStatusP =
    option
        typeReader
        ( long "representation"
            <> help "Status of the new content"
            <> metavar "STRING"
            <> showDefault
            <> value CurrentStatus
        )

optContentRepresentationP :: Parser ContentRepresentation
optContentRepresentationP =
    option
        typeReader
        ( long "representation"
            <> help "Storage format of the content body"
            <> metavar "STRING"
            <> showDefault
            <> value StorageRepresentation
        )

data ContentListOpts = ContentListOpts
    { space :: Maybe SpaceKey
    , title :: Maybe T.Text
    , start :: Int
    , limit :: Int
    }
    deriving (Eq)

-- TODO: Only supports --type "page" for now (because title required for page
-- type, date required for blogpost type)
contentListP :: Parser ConfluenceCmd
contentListP =
    fmap ContentListCommand $
        ContentListOpts
            <$> optional optSpaceKeyP
            <*> optional optContentTitleP
            <*> optStartP
            <*> optLimitP

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
            <$> optStartP
            <*> optLimitP
            <*> optSpaceTypeP

optSpaceTypeP :: Parser (Maybe SpaceType)
optSpaceTypeP =
    optional $
        option
            typeReader
            ( long "type"
                <> help "Filter results to spaces based on their type"
                <> metavar "global|personal"
            )

--------------------------------------------------------------------------------
-- Common options

optStartP :: Parser Int
optStartP =
    option
        auto
        ( long "start"
            <> help "Starting index of the returned spaces"
            <> showDefault
            <> value 0
            <> metavar "INT"
        )

optLimitP :: Parser Int
optLimitP =
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

typeReader :: FromText a => ReadM a
typeReader = maybeReader (fromText . T.pack)

--------------------------------------------------------------------------------
