--------------------------------------------------------------------------------

module Confluence.API (
    -- * Content
    createContent,
    deleteContent,
    getContentById,
    getContentByTitle,
    getContents,
    updateContent,

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
import Prelude hiding (getContents, id)

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
            , "body" .= mkBodyObject repr body
            , "metadata" .= object ["properties" .= object []]
            ]

mkBodyObject :: ContentRepresentation -> T.Text -> Value
mkBodyObject repr body =
    object [AesonKey.fromText (toText repr) .= ContentBody body repr]

deleteContent :: ContentId -> Bool -> ConfluenceM ()
deleteContent id purge =
    deleteApi
        ("content/" <> T.unpack id)
        [("status", toQueryValue (toText TrashedStatus)) | purge]

-- | Returns a list of pages filtered by the space key and title, if given.
-- Title filter uses exact match and is case sensitive.
getContents ::
    Maybe SpaceKey -> Maybe T.Text -> Int -> Int -> ConfluenceM ContentArray
getContents m_key m_title start limit =
    getApi
        "content"
        [ ("spaceKey", toQueryValue m_key)
        , ("title", toQueryValue m_title)
        , ("start", toQueryValue start)
        , ("limit", toQueryValue limit)
        , ("expand", toQueryValue contentExpandableParams)
        ]

contentExpandableParams :: [T.Text]
contentExpandableParams = ["space", "body.storage", "version"]

getContentById :: ContentId -> ConfluenceM (Maybe Content)
getContentById id =
    getApi
        ("content/" <> T.unpack id)
        [("expand", toQueryValue contentExpandableParams)]

getContentByTitle :: SpaceKey -> T.Text -> ConfluenceM (Maybe Content)
getContentByTitle key title = do
    contentArray <- getContents (Just key) (Just title) 0 1
    pure $ headMaybe contentArray.results

updateContent ::
    ContentId ->
    Int ->
    T.Text ->
    ContentType ->
    Maybe ContentStatus ->
    ContentRepresentation ->
    Maybe T.Text ->
    ConfluenceM ()
updateContent id new_version new_title new_ty new_status new_repr new_body =
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
