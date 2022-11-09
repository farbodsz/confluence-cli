--------------------------------------------------------------------------------

module Confluence.API
    ( getSpaces
    ) where

import           Confluence.API.Request
import           Confluence.Monad               ( ConfluenceM )
import           Confluence.Types

--------------------------------------------------------------------------------
-- API endpoint functions

getSpaces :: ConfluenceM SpaceArray
getSpaces = queryApi "space" []

--------------------------------------------------------------------------------
