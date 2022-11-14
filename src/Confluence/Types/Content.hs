--------------------------------------------------------------------------------

module Confluence.Types.Content (
    ) where

import Confluence.Types.Common (GenericLinks)
import Confluence.Types.ResultArray (ResultArray)
import Confluence.Types.Space (Space)
import Data.Aeson (
    FromJSON (parseJSON),
    Object,
    withObject,
    withText,
    (.:),
 )
import Data.Text (Text)
import Network.HTTP.Types.QueryLike (QueryValueLike (toQueryValue))

--------------------------------------------------------------------------------

type ContentArray = ResultArray Content

--------------------------------------------------------------------------------

-- We leave out these fields from Content, as we don't care about them for now:
--
--   * history
--   * version
--   * ancestors
--   * operations
--   * children
--   * childTypes
--   * descendants
--   * container
--   * restrictions
--   * metadata
--   * macroRenderedOutput
--   * extensions
--   * extensions
--
data Content = Content
    { contentId :: Text
    , contentType :: ContentType
    , contentStatus :: Text
    , contentTitle :: Text
    , contentSpace :: Maybe Space
    , contentBody :: ContentBodyContainer
    , contentExpandable :: Object
    , contentLinks :: GenericLinks
    }

data ContentType
    = PageContent
    | BlogpostContent
    | AttachmentContent
    | ContentContent
    deriving (Eq, Show)

instance FromJSON ContentType where
    parseJSON = withText "ContentType" $ \case
        "page" -> PageContent
        "blogpost" -> BlogpostContent
        "attachment" -> AttachmentContent
        "content" -> ContentContent

-- Leaving out "_expandable" field as not needed yet.
data ContentBodyContainer = ContentBodyContainer {
    cbcView :: ContentBody ,
    cbcExportView :: ContentBody,
    cbcStyledView :: ContentBody,
    cbcStorage :: ContentBody,
    cbcWiki :: ContentBody,
    cbcEditor :: ContentBody,
    cbcEditor2 :: ContentBody,
    cbcAnonExportView :: ContentBody,
    cbcAtlasDocFmt :: ContentBody,
    cbcDynamic :: ContentBody
                                                 }

instance FromJSON ContentType where
    parseJSON = withText "ContentType" $ \case
        "page" -> PageContent
        "blogpost" -> BlogpostContent
        "attachment" -> AttachmentContent
        "content" -> ContentContent


--------------------------------------------------------------------------------
