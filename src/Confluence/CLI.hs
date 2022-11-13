--------------------------------------------------------------------------------

module Confluence.CLI (
    getSpaces,
) where

import qualified Confluence.API as API
import Confluence.Config (Config)
import Confluence.Display
import Confluence.Error (
    ResponseError,
    errorMsg,
 )
import Confluence.Monad (runConfluence)
import Confluence.Table
import Confluence.Types
import qualified Data.Text.IO as T

--------------------------------------------------------------------------------
-- CLI functions

getSpaces :: Config -> Int -> Int -> Maybe SpaceType -> IO ()
getSpaces cfg start limit ty = do
    result <- runConfluence cfg $ API.getSpaces start limit ty
    withEither result printSpaces

printSpaces :: SpaceArray -> IO ()
printSpaces arr =
    let spaces = sparrResults arr
     in printTable $
            defaultTable
                [ "ID" : (display . spId <$> spaces)
                , "NAME" : (spName <$> spaces)
                , "KEY" : (spKey <$> spaces)
                , "TYPE" : (display . spType <$> spaces)
                ]

--------------------------------------------------------------------------------
-- Utility functions

-- | @handleErr action@ prints the error message if there is one, else runs the
-- @action@.
withEither :: Either ResponseError a -> (a -> IO ()) -> IO ()
withEither e action = either (T.putStrLn . errorMsg) action e

--------------------------------------------------------------------------------
