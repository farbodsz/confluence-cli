--------------------------------------------------------------------------------

module Confluence.API (
    getSpaces,
) where

import Confluence.API.Request
import Confluence.Monad (ConfluenceM)
import Confluence.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

--------------------------------------------------------------------------------
-- API endpoint functions

-- | @getSpaces start limit type@ returns a list of spaces.
getSpaces :: Int -> Int -> Maybe SpaceType -> ConfluenceM SpaceArray
getSpaces start limit m_ty =
    queryApi
        "space"
        [ ("start", qInt start)
        , ("limit", qInt limit)
        , ("type", qSpaceType <$> m_ty)
        ]

--------------------------------------------------------------------------------
-- Helpers for query items

qInt :: Int -> Maybe ByteString
qInt = Just . B8.pack . show

qSpaceType :: SpaceType -> ByteString
qSpaceType GlobalSpace = "global"
qSpaceType PersonalSpace = "personal"

--------------------------------------------------------------------------------
