--------------------------------------------------------------------------------

module Confluence.CLI (
    -- * Content
    createContent,
    getContentInfo,
    listContent,

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
import Data.Text qualified as T
import Data.Text.IO qualified as T
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

-- | Returns information about a single content.
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
                    [ ["ID", "SPACE", "TITLE", "TYPE", "STATUS"]
                    ,
                        [ content.id
                        , content.space.key <> " (" <> content.space.name <> ")"
                        , content.title
                        , toText $ content.contentType
                        , toText $ content.status
                        ]
                    ]

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

--------------------------------------------------------------------------------
