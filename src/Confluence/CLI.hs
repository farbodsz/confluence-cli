--------------------------------------------------------------------------------

module Confluence.CLI (
    -- * Page
    createPage,
    deletePage,
    getPage,
    listPages,
    updatePage,

    -- * Spaces
    getSpaces,
) where

import Confluence.API qualified as API
import Confluence.Config (Config)
import Confluence.Error (ResponseError, errorMsg)
import Confluence.Monad (runConfluence)
import Confluence.Table
import Confluence.TextConversions (ToText (toText))
import Confluence.Types
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (ZonedTime (zonedTimeToLocalTime), defaultTimeLocale, formatTime)
import System.IO (hFlush, stdout)
import Prelude hiding (id)

--------------------------------------------------------------------------------
-- Page

-- | Creates a Confluence page using with the contents of the given file.
createPage ::
    Config ->
    SpaceKey ->
    T.Text ->
    ContentRepresentation ->
    ContentStatus ->
    FilePath ->
    IO ()
createPage cfg key title repr status path = do
    body <- T.readFile path
    result <-
        runConfluence cfg $
            API.createContent key title repr status PageContent body
    withEither result $ pure . pure ()

deletePage :: Config -> ContentId -> Bool -> IO ()
deletePage cfg id purge = whenM hasUserConfirmed $ do
    result <- runConfluence cfg $ API.deleteContent id purge
    withEither result $ pure . pure ()
  where
    hasUserConfirmed
        | purge = confirm "Are you sure you want to purge this page?"
        | otherwise = pure True

-- | Returns information about a single content.
getPage :: Config -> ContentIdentification -> IO ()
getPage cfg ident = do
    result <- runConfluence cfg $ case ident of
        ContentId id -> API.getContentById id
        ContentName key title -> API.getContentByTitle key title

    withEither result $ \case
        Nothing ->
            T.putStrLn $ case ident of
                ContentId id ->
                    "No page found with ID: '" <> id <> "'"
                ContentName key title ->
                    "No page found: '" <> key <> "' -> '" <> title <> "'"
        Just page -> do
            printTable $
                defaultTable
                    [
                        [ "ID"
                        , "SPACE"
                        , "TITLE"
                        , "STATUS"
                        , "LAST MODIFIED BY"
                        , "LAST MODIFIED AT"
                        , "VERSION"
                        ]
                    ,
                        [ page.id
                        , page.space.key <> " (" <> page.space.name <> ")"
                        , page.title
                        , toText $ page.status
                        , toText $
                            page.version.by.displayName
                                <> " ("
                                <> page.version.by.username
                                <> ")"
                        , fmtWhen page.version.when
                        , toText $ page.version.number
                        ]
                    ]
  where
    fmtWhen :: ZonedTime -> T.Text
    fmtWhen =
        T.pack
            . formatTime defaultTimeLocale "%d %b %Y at %R"
            . zonedTimeToLocalTime

-- | Lists the pages satisfying the given filters.
listPages :: Config -> Maybe SpaceKey -> Maybe T.Text -> Int -> Int -> IO ()
listPages cfg m_key m_title start limit = do
    result <- runConfluence cfg $ API.getContents m_key m_title start limit
    withEither result $ \contentArray ->
        let pages = contentArray.results
            getSpaceKey = (.key) <$> (.space)
         in printTable $
                defaultTable
                    [ "ID" : toTextF ((.id) <$> pages)
                    , "STATUS" : toTextF ((.status) <$> pages)
                    , "SPACE" : toTextF (getSpaceKey <$> pages)
                    , "TITLE" : toTextF ((.title) <$> pages)
                    ]

-- | Updates some metadata or body of the page with the given ID.
updatePage ::
    Config ->
    ContentId ->
    Maybe T.Text ->
    Maybe ContentStatus ->
    ContentRepresentation ->
    Maybe FilePath ->
    IO ()
updatePage cfg id new_title new_status new_repr m_path = do
    new_body <- traverse T.readFile m_path
    result <- runConfluence cfg $ do
        curr_page <- API.getContentById id
        case curr_page of
            Nothing ->
                liftIO . T.putStrLn $
                    "Unable to find existing content with ID: " <> id
            Just page ->
                let new_version = page.version.number + 1
                    new_title' = fromMaybe page.title new_title
                 in API.updateContent
                        id
                        new_version
                        new_title'
                        PageContent
                        new_status
                        new_repr
                        new_body
    withEither result $ pure . pure ()

--------------------------------------------------------------------------------
-- Spaces

-- TODO: support more options:
-- https://developer.atlassian.com/cloud/confluence/rest/v1/api-group-space/#api-wiki-rest-api-space-get
getSpaces :: Config -> Int -> Int -> Maybe SpaceType -> IO ()
getSpaces cfg start limit ty = do
    result <- runConfluence cfg $ API.getSpaces start limit ty
    withEither result printSpaces

printSpaces :: SpaceArray -> IO ()
printSpaces arr =
    let spaces = arr.results
     in printTable $
            defaultTable
                [ "ID" : (toText . (.id) <$> spaces)
                , "NAME" : ((.name) <$> spaces)
                , "KEY" : ((.key) <$> spaces)
                , "TYPE" : (toText . (.spaceType) <$> spaces)
                ]

--------------------------------------------------------------------------------
-- Utility functions

-- | @handleErr action@ prints the error message if there is one, else runs the
-- @action@.
withEither :: Either ResponseError a -> (a -> IO ()) -> IO ()
withEither e action = either (T.putStrLn . ("Error:  " <>) . errorMsg) action e

toTextF :: (Functor f, ToText a) => f a -> f T.Text
toTextF = fmap toText

confirm :: T.Text -> IO Bool
confirm msg = do
    T.putStr $ msg <> " [Y/n]: "
    hFlush stdout
    ans <- T.getLine
    pure $ T.toLower ans == "y"

--------------------------------------------------------------------------------
