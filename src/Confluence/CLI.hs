--------------------------------------------------------------------------------

module Confluence.CLI (
    -- * Content
    createContent,
    getContent,

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

--------------------------------------------------------------------------------
-- Content

-- | @createContent cfg key title contentType filePath@ creates a Confluence
-- page using "storage" representation with the content given by the file path.
createContent ::
    Config -> SpaceKey -> T.Text -> ContentType -> FilePath -> IO ()
createContent cfg key title ty path = do
    body <- T.readFile path
    result <- runConfluence cfg $ API.createContent key title ty body
    withEither result $ pure . pure ()

-- | @getContent cfg m_key m_title start limit@ lists the Confluence /pages/
-- satisfying the given filters.
getContent :: Config -> Maybe SpaceKey -> Maybe T.Text -> Int -> Int -> IO ()
getContent cfg m_key m_title start limit = do
    result <- runConfluence cfg $ API.getContent m_key m_title start limit
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
