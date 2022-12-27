--------------------------------------------------------------------------------

module Confluence.Commands (
    -- * Root-level commands
    cliArgs,
    ConfluenceCmd (..),

    -- * Content command options
    ContentBodyOpts (..),
    ContentCreateOpts (..),
    ContentDeleteOpts (..),
    ContentInfoOpts (..),
    ContentIdentification (..),
    ContentListOpts (..),
    ContentUpdateOpts (..),

    -- * Spaces command options
    SpacesListOpts (..),
) where

import Confluence.TextConversions (FromText (fromText), ToText (toText))
import Confluence.Types
import Data.Text qualified as T
import Data.Version (showVersion)
import Options.Applicative
import Paths_confluence_cli (version)

--------------------------------------------------------------------------------
-- Root

data ConfluenceCmd
    = ContentBodyCommand ContentBodyOpts
    | ContentCreateCommand ContentCreateOpts
    | ContentDeleteCommand ContentDeleteOpts
    | ContentInfoCommand ContentInfoOpts
    | ContentListCommand ContentListOpts
    | ContentUpdateCommand ContentUpdateOpts
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
            <> command "body" (info contentBodyP $ progDesc "Get content body")
            <> command "info" (info contentInfoP $ progDesc "Content info")
            <> command "list" (info contentListP $ progDesc "List content")
            <> command "rm" (info contentDeleteP $ progDesc "Delete content")
            <> command "update" (info contentUpdateP $ progDesc "Update content")

data ContentBodyOpts = ContentBodyOpts
    { space :: SpaceKey
    , title :: T.Text
    }
    deriving (Eq)

contentBodyP :: Parser ConfluenceCmd
contentBodyP =
    fmap ContentBodyCommand $
        ContentBodyOpts <$> optSpaceKeyP <*> optContentTitleP

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
            <*> optFilePathP
            <*> optContentTypeP
            <*> optContentStatusP
            <*> optContentRepresentationP

data ContentDeleteOpts = ContentDeleteOpts
    { id :: ContentId
    , purge :: Bool
    }
    deriving (Eq)

contentDeleteP :: Parser ConfluenceCmd
contentDeleteP =
    fmap ContentDeleteCommand $
        ContentDeleteOpts <$> argContentIdP <*> flagContentPurgeP

data ContentInfoOpts = ContentInfoOpts {ident :: ContentIdentification}
    deriving (Eq)

contentInfoP :: Parser ConfluenceCmd
contentInfoP =
    fmap ContentInfoCommand $
        ContentInfoOpts <$> (contentIdP <|> contentNameP)
  where
    contentIdP = ContentId <$> optContentIdP
    contentNameP = ContentName <$> argSpaceKeyP <*> argContentTitleP

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

data ContentUpdateOpts = ContentUpdateOpts
    { id :: ContentId
    , newTitle :: Maybe T.Text
    , newType :: ContentType
    , newStatus :: Maybe ContentStatus
    , newRepresentation :: ContentRepresentation
    , newBodyFilePath :: Maybe FilePath
    }
    deriving (Eq)

contentUpdateP :: Parser ConfluenceCmd
contentUpdateP =
    fmap ContentUpdateCommand $
        ContentUpdateOpts
            <$> argContentIdP
            <*> optional optContentTitleP
            <*> optContentTypeP
            <*> optional optContentStatusP
            <*> optContentRepresentationP
            <*> optional optFilePathP

--------------------------------------------------------------------------------
-- Content arguments

argContentIdP :: Parser ContentId
argContentIdP = strArgument (help "Content ID" <> metavar "ID")

argSpaceKeyP :: Parser SpaceKey
argSpaceKeyP =
    strArgument (help "Space that the content belongs to" <> metavar "SPACE")

argContentTitleP :: Parser T.Text
argContentTitleP = strArgument (help "Content title" <> metavar "TITLE")

--------------------------------------------------------------------------------
-- Content options

optContentIdP :: Parser ContentId
optContentIdP = strOption (long "id" <> help "Content ID" <> metavar "ID")

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
            <> showDefaultWith (T.unpack . toText)
            <> value PageContent
        )

optContentStatusP :: Parser ContentStatus
optContentStatusP =
    option
        typeReader
        ( long "status"
            <> help "Status of the new content"
            <> metavar "STRING"
            <> showDefaultWith (T.unpack . toText)
            <> value CurrentStatus
        )

optContentRepresentationP :: Parser ContentRepresentation
optContentRepresentationP =
    option
        typeReader
        ( long "representation"
            <> help "Storage format of the content body"
            <> metavar "STRING"
            <> showDefaultWith (T.unpack . toText)
            <> value StorageRepresentation
        )

flagContentPurgeP :: Parser Bool
flagContentPurgeP =
    switch
        ( long "purge"
            <> help
                "If a content's type is `page` or `blogpost` and it's already\
                \ `trashed`, then set this flag to purge it from the trash and\
                \ delete permanently. Comments or attachments are deleted\
                \ permanently regardless of whether this flag is set."
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
            <$> optStartP
            <*> optLimitP
            <*> optSpaceTypeP

--------------------------------------------------------------------------------
-- Space options

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

optFilePathP :: Parser FilePath
optFilePathP =
    strOption
        ( long "path"
            <> metavar "FILEPATH"
            <> help "Path to input file"
        )

--------------------------------------------------------------------------------
-- Option parsing for custom types

typeReader :: FromText a => ReadM a
typeReader = maybeReader (fromText . T.pack)

--------------------------------------------------------------------------------
