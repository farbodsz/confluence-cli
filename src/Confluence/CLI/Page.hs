--------------------------------------------------------------------------------

-- | Page-related actions.
module Confluence.CLI.Page (
    createPage,
    deletePage,
    getPage,
    getPageBody,
    listPages,
    listPageChildren,
    openPage,
    updatePage,
) where

import Confluence.API.Endpoints qualified as API
import Confluence.API.Types
import Confluence.CLI.Table
import Confluence.CLI.Types
import Confluence.CLI.Util
import Confluence.Config (Config)
import Confluence.Monad (runConfluence)
import Confluence.TextConversions (ToText (toText))
import Control.Monad.Extra (void, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (
    ZonedTime (zonedTimeToLocalTime),
    defaultTimeLocale,
    formatTime,
 )
import Web.Browser (openBrowser)
import Prelude hiding (id)

--------------------------------------------------------------------------------

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
                        , "WEB"
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
                        , fromMaybe "" (getWebLink page._links)
                        ]
                    ]
  where
    fmtWhen :: ZonedTime -> T.Text
    fmtWhen =
        T.pack
            . formatTime defaultTimeLocale "%d %b %Y at %R"
            . zonedTimeToLocalTime

-- | Outputs the content body in storage representation.
getPageBody :: Config -> SpaceKey -> T.Text -> IO ()
getPageBody cfg key title = do
    result <- runConfluence cfg $ API.getContentByTitle key title

    let errTxt x = T.concat ["No ", x, " found: '", key, "' -> '", title, "'"]
    withEither result $ \case
        Nothing -> T.putStrLn $ errTxt "content"
        Just content -> case content.body.storage of
            Nothing -> T.putStrLn $ errTxt "content body (storage)"
            Just storage -> T.putStrLn storage.value

-- | Lists the direct child pages of the specified page.
listPageChildren :: Config -> ContentIdentification -> IO ()
listPageChildren cfg ident = do
    mContentId <- case ident of
        ContentId id -> pure . pure $ Just id
        ContentName key title ->
            runConfluence cfg $ fmap (.id) <$> API.getContentByTitle key title
    withEither mContentId $ \case
        Nothing -> T.putStrLn "No page found matching given key/title"
        Just id -> do
            result <- runConfluence cfg $ API.getContentChildren id
            withEither result $ \contentChildren ->
                printPages contentChildren.page.results

-- | Lists the pages satisfying the given filters.
listPages :: Config -> Maybe SpaceKey -> Maybe T.Text -> Int -> Int -> IO ()
listPages cfg m_key m_title start limit = do
    result <- runConfluence cfg $ API.getContents m_key m_title start limit
    withEither result $ \contentArray -> printPages contentArray.results

-- | Open a page in the user's preferred browser.
openPage :: Config -> ContentIdentification -> IO ()
openPage cfg ident = do
    result <- runConfluence cfg $ case ident of
        ContentId id -> API.getContentById id
        ContentName key title -> API.getContentByTitle key title

    withEither result $ \case
        Nothing -> putStrLn "No page found"
        Just page ->
            let webLink = getWebLink page._links
             in case webLink of
                    Nothing -> putStrLn "This page has no web URL"
                    Just url -> void $ openBrowser (T.unpack url)

-- | Prints a table of pages.
printPages :: [Content] -> IO ()
printPages pages =
    printTable $
        defaultTable
            [ "ID" : toTextF ((.id) <$> pages)
            , "STATUS" : toTextF ((.status) <$> pages)
            , "SPACE" : toTextF (getSpaceKey <$> pages)
            , "TITLE" : toTextF ((.title) <$> pages)
            ]
  where
    getSpaceKey = (.key) <$> (.space)

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
