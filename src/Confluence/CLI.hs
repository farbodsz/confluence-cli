--------------------------------------------------------------------------------

module Confluence.CLI (
    getSpaces,
) where

import Confluence.API qualified as API
import Confluence.Config (Config)
import Confluence.Display
import Confluence.Error (
    ResponseError,
    errorMsg,
 )
import Confluence.Monad (runConfluence)
import Confluence.Table
import Confluence.Types
import Data.Text.IO qualified as T

--------------------------------------------------------------------------------
-- CLI functions

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
                [ "ID" : (display . (.id) <$> spaces)
                , "NAME" : ((.name) <$> spaces)
                , "KEY" : ((.key) <$> spaces)
                , "TYPE" : (display . (.spaceType) <$> spaces)
                ]

--------------------------------------------------------------------------------
-- Utility functions

-- | @handleErr action@ prints the error message if there is one, else runs the
-- @action@.
withEither :: Either ResponseError a -> (a -> IO ()) -> IO ()
withEither e action = either (T.putStrLn . ("Error:  " <>) . errorMsg) action e

--------------------------------------------------------------------------------
