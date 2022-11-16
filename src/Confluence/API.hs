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
import Confluence.Types
import Data.Aeson (object, (.=))
import Data.Text qualified as T
import Network.HTTP.Types.QueryLike (QueryValueLike (toQueryValue))

--------------------------------------------------------------------------------
-- Content

-- | @createContent spaceKey title contentType body@ creates a page.
createContent :: SpaceKey -> T.Text -> ContentType -> T.Text -> ConfluenceM ()
createContent key title ty body =
    postApi
        "content"
        $ object
            [ "space" .= object ["key" .= key]
            , "title" .= title
            , "type" .= ty
            , "body" .= representationObj
            , "metadata" .= object ["properties" .= object []]
            ]
  where
    -- TODO: figure out how to use ToJSONKey for this and let representation be
    -- modifiable
    representationObj =
        object
            [ "storage"
                .= ContentBodyCreate body StorageRepresentation
            ]

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
