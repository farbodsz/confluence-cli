--------------------------------------------------------------------------------

-- | Space-related actions.
module Confluence.CLI.Space (
    getSpaces,
) where

import Confluence.API.Endpoints qualified as API
import Confluence.API.Types
import Confluence.CLI.Table
import Confluence.CLI.Util
import Confluence.Config (Config)
import Confluence.Monad (runConfluence)
import Confluence.TextConversions (ToText (toText))

--------------------------------------------------------------------------------

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
