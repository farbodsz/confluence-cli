--------------------------------------------------------------------------------

module Confluence.CLI (
    -- * Content
    createContent,
    deleteContent,
    getContentBody,
    getContentInfo,
    listContent,
    updateContent,

    -- * Spaces
    getSpaces,
) where

import Confluence.API qualified as API
import Confluence.Config (Config)
import Confluence.Error (
    ResponseError,
    errorMsg,
 )
import Confluence.Monad (runConfluence)
import Confluence.Table
import Confluence.TextConversions (ToText (toText))
import Confluence.Types
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (
    ZonedTime (zonedTimeToLocalTime),
    defaultTimeLocale,
    formatTime,
 )
import System.IO (hFlush, stdout)
import Prelude hiding (id)

--------------------------------------------------------------------------------
-- Content

-- | Creates a Confluence page using with the contents of the given file.
createContent ::
    Config ->
    SpaceKey ->
    T.Text ->
    ContentRepresentation ->
    ContentStatus ->
    ContentType ->
    FilePath ->
    IO ()
createContent cfg key title repr status ty path = do
    body <- T.readFile path
    result <-
        runConfluence cfg $
            API.createContent
                key
                title
                repr
                status
                ty
                body
    withEither result $ pure . pure ()

deleteContent :: Config -> ContentId -> Bool -> IO ()
deleteContent cfg id purge = whenM hasUserConfirmed $ do
    result <- runConfluence cfg $ API.deleteContent id purge
    withEither result $ pure . pure ()
  where
    hasUserConfirmed
        | purge = confirm "Are you sure you want to purge this content?"
        | otherwise = pure True

-- | Outputs the content body in storage representation.
getContentBody :: Config -> SpaceKey -> T.Text -> IO ()
getContentBody cfg key title = do
    result <- runConfluence cfg $ API.getContentByTitle key title

    let errTxt x = T.concat ["No ", x, " found: '", key, "' -> '", title, "'"]
    withEither result $ \case
        Nothing -> T.putStrLn $ errTxt "content"
        Just content -> case content.body.storage of
            Nothing -> T.putStrLn $ errTxt "content body (storage)"
            Just storage -> T.putStrLn storage.value

-- | Prints information about a single content.
getContentInfo :: Config -> ContentIdentification -> IO ()
getContentInfo cfg ident = do
    result <- runConfluence cfg $ case ident of
        ContentId id -> API.getContentById id
        ContentName key title -> API.getContentByTitle key title

    withEither result $ \case
        Nothing ->
            T.putStrLn $ case ident of
                ContentId id ->
                    "No content found with ID: '" <> id <> "'"
                ContentName key title ->
                    "No content found: '" <> key <> "' -> '" <> title <> "'"
        Just content -> do
            printTable $
                defaultTable
                    [
                        [ "ID"
                        , "SPACE"
                        , "TITLE"
                        , "TYPE"
                        , "STATUS"
                        , "LAST MODIFIED BY"
                        , "LAST MODIFIED AT"
                        , "VERSION"
                        ]
                    ,
                        [ content.id
                        , content.space.key <> " (" <> content.space.name <> ")"
                        , content.title
                        , toText $ content.contentType
                        , toText $ content.status
                        , toText $
                            content.version.by.displayName
                                <> " ("
                                <> content.version.by.username
                                <> ")"
                        , fmtWhen content.version.when
                        , toText $ content.version.number
                        ]
                    ]
  where
    fmtWhen :: ZonedTime -> T.Text
    fmtWhen =
        T.pack
            . formatTime defaultTimeLocale "%d %b %Y at %R"
            . zonedTimeToLocalTime

-- | Lists the content satisfying the given filters.
listContent :: Config -> Maybe SpaceKey -> Maybe T.Text -> Int -> Int -> IO ()
listContent cfg m_key m_title start limit = do
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

-- | Update content modifies some metadata or body of the content with the given
-- ID.
updateContent ::
    Config ->
    ContentId ->
    Maybe T.Text ->
    ContentType ->
    Maybe ContentStatus ->
    ContentRepresentation ->
    Maybe FilePath ->
    IO ()
updateContent cfg id new_title new_ty new_status new_repr m_path = do
    new_body <- traverse T.readFile m_path
    result <- runConfluence cfg $ do
        curr_content <- API.getContentById id
        case curr_content of
            Nothing ->
                liftIO . T.putStrLn $
                    "Unable to find existing content with ID: " <> id
            Just content ->
                let new_version = content.version.number + 1
                    new_title' = fromMaybe content.title new_title
                 in API.updateContent
                        id
                        new_version
                        new_title'
                        new_ty
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
