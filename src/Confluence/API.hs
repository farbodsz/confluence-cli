--------------------------------------------------------------------------------

module Confluence.API (
    -- * Content
    createContent,

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
    representationObj =
        let reprType = "storage"
         in object
                [ reprType
                    .= object
                        [ "value" .= body
                        , "representation" .= reprType
                        ]
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
