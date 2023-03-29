--------------------------------------------------------------------------------

module Confluence.Commands (
    -- * Root-level commands
    cliArgs,
    ConfluenceCmd (..),

    -- * Page command options
    PageCreateOpts (..),
    PageDeleteOpts (..),
    PageInfoOpts (..),
    PageIdentification (..),
    PageListOpts (..),
    PageUpdateOpts (..),

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
    = PageCreateCmd PageCreateOpts
    | PageDeleteCmd PageDeleteOpts
    | PageInfoCmd PageInfoOpts
    | PageListCmd PageListOpts
    | PageUpdateCmd PageUpdateOpts
    | SpacesListCmd SpacesListOpts
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
        command "page" (info pageP $ progDesc "Pages")
            <> command "space" (info spacesP $ progDesc "Spaces")

--------------------------------------------------------------------------------
-- Page

pageP :: Parser ConfluenceCmd
pageP =
    hsubparser $
        command "create" (info pageCreateP $ progDesc "Create page")
            <> command "info" (info pageInfoP $ progDesc "Get page")
            <> command "list" (info pageListP $ progDesc "List pages")
            <> command "rm" (info pageDeleteP $ progDesc "Delete page")
            <> command "update" (info pageUpdateP $ progDesc "Update page")

data PageCreateOpts = PageCreateOpts
    { space :: SpaceKey
    , title :: T.Text
    , filePath :: FilePath
    , contentType :: ContentType
    , status :: PageStatus
    , representation :: ContentRepresentation
    }
    deriving (Eq)

pageCreateP :: Parser ConfluenceCmd
pageCreateP =
    fmap PageCreateCmd $
        PageCreateOpts
            <$> optSpaceKeyP
            <*> optPageTitleP
            <*> optFilePathP
            <*> optContentTypeP
            <*> optPageStatusP
            <*> optContentRepresentationP

data PageDeleteOpts = PageDeleteOpts
    { id :: PageId
    , purge :: Bool
    }
    deriving (Eq)

pageDeleteP :: Parser ConfluenceCmd
pageDeleteP =
    fmap PageDeleteCmd $
        PageDeleteOpts <$> argPageIdP <*> flagPagePurgeP

data PageInfoOpts = PageInfoOpts {ident :: PageIdentification}
    deriving (Eq)

pageInfoP :: Parser ConfluenceCmd
pageInfoP =
    fmap PageInfoCmd $
        PageInfoOpts <$> (pageIdP <|> pageNameP)
  where
    pageIdP = PageId <$> optPageIdP
    pageNameP = PageName <$> argSpaceKeyP <*> argPageTitleP

data PageListOpts = PageListOpts
    { space :: Maybe SpaceKey
    , title :: Maybe T.Text
    , start :: Int
    , limit :: Int
    }
    deriving (Eq)

-- TODO: Only supports --type "page" for now (because title required for page
-- type, date required for blogpost type)
pageListP :: Parser ConfluenceCmd
pageListP =
    fmap PageListCmd $
        PageListOpts
            <$> optional optSpaceKeyP
            <*> optional optPageTitleP
            <*> optStartP
            <*> optLimitP

data PageUpdateOpts = PageUpdateOpts
    { id :: PageId
    , newTitle :: Maybe T.Text
    , newType :: ContentType
    , newStatus :: Maybe PageStatus
    , newRepresentation :: ContentRepresentation
    , newBodyFilePath :: Maybe FilePath
    }
    deriving (Eq)

pageUpdateP :: Parser ConfluenceCmd
pageUpdateP =
    fmap PageUpdateCmd $
        PageUpdateOpts
            <$> argPageIdP
            <*> optional optPageTitleP
            <*> optContentTypeP
            <*> optional optPageStatusP
            <*> optContentRepresentationP
            <*> optional optFilePathP

--------------------------------------------------------------------------------
-- Page arguments

argPageIdP :: Parser PageId
argPageIdP = strArgument (help "Page ID" <> metavar "ID")

argSpaceKeyP :: Parser SpaceKey
argSpaceKeyP =
    strArgument (help "Space that the page belongs to" <> metavar "SPACE")

argPageTitleP :: Parser T.Text
argPageTitleP = strArgument (help "Page title" <> metavar "TITLE")

--------------------------------------------------------------------------------
-- Page options

optPageIdP :: Parser PageId
optPageIdP = strOption (long "id" <> help "Page ID" <> metavar "ID")

optSpaceKeyP :: Parser SpaceKey
optSpaceKeyP =
    strOption
        ( long "space"
            <> help "Space that the page belongs to"
            <> metavar "STRING"
        )

optPageTitleP :: Parser T.Text
optPageTitleP =
    strOption
        ( long "title"
            <> help "Page title"
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

optPageStatusP :: Parser PageStatus
optPageStatusP =
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

flagPagePurgeP :: Parser Bool
flagPagePurgeP =
    switch
        ( long "purge"
            <> help
                "Set this flag to purge it from the trash and delete\
                \ permanently. Comments or attachments are deleted permanently\
                \ regardless of whether this flag is set."
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
    fmap SpacesListCmd $
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
