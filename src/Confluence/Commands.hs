--------------------------------------------------------------------------------

module Confluence.Commands (
    -- * Root-level commands
    cliArgs,
    ConfluenceCmd (..),

    -- * Page command options
    PageBodyOpts (..),
    PageChildrenOpts (..),
    PageCreateOpts (..),
    PageDeleteOpts (..),
    PageGetOpts (..),
    PageListOpts (..),
    PageUpdateOpts (..),

    -- * Spaces command options
    SpacesListOpts (..),
) where

import Confluence.API.Types
import Confluence.CLI.Types (ContentIdentification (..))
import Confluence.TextConversions (FromText (fromText), ToText (toText))
import Data.Text qualified as T
import Data.Version (showVersion)
import Options.Applicative
import Paths_confluence_cli (version)

--------------------------------------------------------------------------------
-- Root

data ConfluenceCmd
    = PageBodyCommand PageBodyOpts
    | PageChildrenCommand PageChildrenOpts
    | PageCreateCommand PageCreateOpts
    | PageDeleteCommand PageDeleteOpts
    | PageGetCommand PageGetOpts
    | PageListCommand PageListOpts
    | PageUpdateCommand PageUpdateOpts
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
        command "page" (info pageP $ progDesc "Page")
            <> command "space" (info spacesP $ progDesc "Spaces")

--------------------------------------------------------------------------------
-- Page

pageP :: Parser ConfluenceCmd
pageP =
    hsubparser $
        command "add" (info pageCreateP $ progDesc "Create a new page")
            <> command
                "body"
                ( info pageBodyP $
                    progDesc "Get the body of a page in storage represntation format"
                )
            <> command
                "children"
                ( info pageChildrenP $
                    progDesc "Get the direct children for a page"
                )
            <> command
                "get"
                ( info pageGetP $
                    progDesc "Get properties of an existing page"
                )
            <> command "list" (info pageListP $ progDesc "List pages")
            <> command "rm" (info pageDeleteP $ progDesc "Delete a page")
            <> command "update" (info pageUpdateP $ progDesc "Update a page")

data PageBodyOpts = PageBodyOpts
    { space :: SpaceKey
    , title :: T.Text
    }
    deriving (Eq)

pageBodyP :: Parser ConfluenceCmd
pageBodyP =
    fmap PageBodyCommand $ PageBodyOpts <$> optSpaceKeyP <*> optContentTitleP

data PageChildrenOpts = PageChildrenOpts {ident :: ContentIdentification}
    deriving (Eq)

pageChildrenP :: Parser ConfluenceCmd
pageChildrenP = fmap PageChildrenCommand $ PageChildrenOpts <$> argContentIdentificationP

data PageCreateOpts = PageCreateOpts
    { space :: SpaceKey
    , title :: T.Text
    , filePath :: FilePath
    , status :: ContentStatus
    , representation :: ContentRepresentation
    }
    deriving (Eq)

pageCreateP :: Parser ConfluenceCmd
pageCreateP =
    fmap PageCreateCommand $
        PageCreateOpts
            <$> optSpaceKeyP
            <*> optContentTitleP
            <*> optFilePathP
            <*> optContentStatusP
            <*> optContentRepresentationP

data PageDeleteOpts = PageDeleteOpts
    { id :: ContentId
    , purge :: Bool
    }
    deriving (Eq)

pageDeleteP :: Parser ConfluenceCmd
pageDeleteP =
    fmap PageDeleteCommand $
        PageDeleteOpts <$> argContentIdP <*> flagContentPurgeP

data PageGetOpts = PageGetOpts {ident :: ContentIdentification}
    deriving (Eq)

pageGetP :: Parser ConfluenceCmd
pageGetP = fmap PageGetCommand $ PageGetOpts <$> argContentIdentificationP

data PageListOpts = PageListOpts
    { space :: Maybe SpaceKey
    , title :: Maybe T.Text
    , start :: Int
    , limit :: Int
    }
    deriving (Eq)

pageListP :: Parser ConfluenceCmd
pageListP =
    fmap PageListCommand $
        PageListOpts
            <$> optional optSpaceKeyP
            <*> optional optContentTitleP
            <*> optStartP
            <*> optLimitP

data PageUpdateOpts = PageUpdateOpts
    { id :: ContentId
    , newTitle :: Maybe T.Text
    , newStatus :: Maybe ContentStatus
    , newRepresentation :: ContentRepresentation
    , newBodyFilePath :: Maybe FilePath
    }
    deriving (Eq)

pageUpdateP :: Parser ConfluenceCmd
pageUpdateP =
    fmap PageUpdateCommand $
        PageUpdateOpts
            <$> argContentIdP
            <*> optional optContentTitleP
            <*> optional optContentStatusP
            <*> optContentRepresentationP
            <*> optional optFilePathP

--------------------------------------------------------------------------------
-- Content ID

argContentIdP :: Parser ContentId
argContentIdP = strArgument (help "Content ID" <> metavar "ID")

-- | Content ID or name.
argContentIdentificationP :: Parser ContentIdentification
argContentIdentificationP = contentIdP <|> contentNameP
  where
    contentIdP = ContentId <$> optContentIdP
    contentNameP = ContentName <$> optSpaceKeyP <*> optContentTitleP

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
