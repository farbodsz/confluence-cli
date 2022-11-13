--------------------------------------------------------------------------------

module Confluence.API (
    getSpaces,
) where

import Confluence.API.Request
import Confluence.Monad (ConfluenceM)
import Confluence.Types
import Network.HTTP.Types.QueryLike (QueryValueLike (toQueryValue))

--------------------------------------------------------------------------------
-- API endpoint functions

-- | @getSpaces start limit type@ returns a list of spaces.
getSpaces :: Int -> Int -> Maybe SpaceType -> ConfluenceM SpaceArray
getSpaces start limit m_ty =
    queryApi
        "space"
        [ ("start", toQueryValue start)
        , ("limit", toQueryValue limit)
        , ("type", toQueryValue m_ty)
        ]

--------------------------------------------------------------------------------
