--------------------------------------------------------------------------------

module Confluence.API (
    -- * Content
    createContent,
    getContent,

    -- * Spaces
    getSpaces,
) where

import Confluence.API.Request
import Confluence.Monad (ConfluenceM)
import Confluence.TextConversions (toText)
import Confluence.Types
import Data.Aeson (object, (.=))
import Data.Aeson.Key qualified as AesonKey
import Data.Text qualified as T
import Network.HTTP.Types.QueryLike (QueryValueLike (toQueryValue))

--------------------------------------------------------------------------------
-- Content

-- | Creates a page.
createContent ::
    SpaceKey ->
    T.Text ->
    ContentRepresentation ->
    ContentStatus ->
    ContentType ->
    T.Text ->
    ConfluenceM ()
createContent key title repr status ty body =
    postApi
        "content"
        $ object
            [ "space" .= object ["key" .= key]
            , "title" .= title
            , "type" .= ty
            , "status" .= status
            , "body" .= representationObj
            , "metadata" .= object ["properties" .= object []]
            ]
  where
    representationObj =
        object [AesonKey.fromText (toText repr) .= ContentBodyCreate body repr]

-- | @getContent m_spaceKey m_title start limit@ returns a list of pages
-- filtered by the space key and title, if given. Title filter uses exact match
-- and is case sensitive.
getContent ::
    Maybe SpaceKey -> Maybe T.Text -> Int -> Int -> ConfluenceM ContentArray
getContent m_key m_title start limit =
    getApi
        "content"
        [ ("spaceKey", toQueryValue m_key)
        , ("title", toQueryValue m_title)
        , ("start", toQueryValue start)
        , ("limit", toQueryValue limit)
        , ("expand", Just "space")
        ]

--------------------------------------------------------------------------------
-- Spaces

-- | @getSpaces start limit type@ returns a list of spaces.
getSpaces :: Int -> Int -> Maybe SpaceType -> ConfluenceM SpaceArray
getSpaces start limit m_ty =
    getApi
        "space"
        [ ("start", toQueryValue start)
        , ("limit", toQueryValue limit)
        , ("type", toQueryValue m_ty)
        ]

--------------------------------------------------------------------------------
