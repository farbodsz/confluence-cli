--------------------------------------------------------------------------------

-- | Commands for dealing with objects in Confluence.
module Confluence.API (
    -- * Page
    createPage,
    deletePage,
    getPageById,
    getPageByTitle,
    getPages,
    updatePage,

    -- * Spaces
    getSpaces,
) where

import Confluence.API.Request
import Confluence.Monad (ConfluenceM)
import Confluence.TextConversions (toText)
import Confluence.Types
import Confluence.Util (headMaybe)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Key qualified as AesonKey
import Data.Text qualified as T
import Network.HTTP.Types.QueryLike (QueryValueLike (toQueryValue))
import Prelude hiding (id)

--------------------------------------------------------------------------------
-- Page

-- | Creates a page.
createPage ::
    SpaceKey ->
    T.Text ->
    ContentRepresentation ->
    PageStatus ->
    ContentType ->
    T.Text ->
    ConfluenceM ()
createPage key title repr status ty body =
    postApi
        "content"
        $ object
            [ "space" .= object ["key" .= key]
            , "title" .= title
            , "type" .= ty
            , "status" .= status
            , "body" .= mkBodyObject repr body
            , "metadata" .= object ["properties" .= object []]
            ]

mkBodyObject :: ContentRepresentation -> T.Text -> Value
mkBodyObject repr body =
    object
        [ AesonKey.fromText (toText repr) .= ContentBodyCreate body repr
        ]

deletePage :: PageId -> Bool -> ConfluenceM ()
deletePage id purge =
    deleteApi
        ("content/" <> T.unpack id)
        [("status", toQueryValue (toText TrashedStatus)) | purge]

-- | Returns a list of pages filtered by the space key and title, if given.
-- Title filter uses exact match and is case sensitive.
getPages ::
    Maybe SpaceKey -> Maybe T.Text -> Int -> Int -> ConfluenceM ContentArray
getPages m_key m_title start limit =
    getApi
        "content"
        [ ("spaceKey", toQueryValue m_key)
        , ("title", toQueryValue m_title)
        , ("start", toQueryValue start)
        , ("limit", toQueryValue limit)
        , ("expand", toQueryValue @[T.Text] ["space", "version"])
        ]

getPageById :: PageId -> ConfluenceM (Maybe Content)
getPageById id = getApi ("content/" <> T.unpack id) []

getPageByTitle :: SpaceKey -> T.Text -> ConfluenceM (Maybe Content)
getPageByTitle key title = do
    contentArray <- getPages (Just key) (Just title) 0 1
    pure $ headMaybe contentArray.results

updatePage ::
    PageId ->
    Int ->
    T.Text ->
    ContentType ->
    Maybe PageStatus ->
    ContentRepresentation ->
    Maybe T.Text ->
    ConfluenceM ()
updatePage id new_version new_title new_ty new_status new_repr new_body =
    putApi ("content/" <> T.unpack id) $
        object $ case new_body of
            Nothing ->
                [ "version" .= object ["number" .= new_version]
                , "type" .= new_ty
                , "title" .= new_title
                , "status" .= new_status
                ]
            Just b ->
                [ "version" .= object ["number" .= new_version]
                , "type" .= new_ty
                , "title" .= new_title
                , "status" .= new_status
                , "body" .= mkBodyObject new_repr b
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
