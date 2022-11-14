--------------------------------------------------------------------------------

module Confluence.Types.Content (
    ContentArray,
    Content (..),
    ContentType (..),
    ContentStatus (..),
    ContentBodyContainer (..),
    ContentBody (..),
    ContentRepresentation (..),
) where

import Confluence.Types.Common (GenericLinks)
import Confluence.Types.ResultArray (ResultArray)
import Confluence.Types.Space (Space)
import Data.Aeson (
    FromJSON (parseJSON),
    Object,
    ToJSON (toJSON),
    withObject,
    withText,
    (.:),
    (.:?),
 )
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

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
    { id :: Text
    , contentType :: ContentType
    , status :: ContentStatus
    , title :: Text
    , space :: Maybe Space
    , body :: ContentBodyContainer
    , _expandable :: Object
    , _links :: GenericLinks
    }
    deriving (Show)

instance FromJSON Content where
    parseJSON = withObject "Content" $ \v ->
        Content
            <$> (v .: "id")
            <*> (v .: "type")
            <*> (v .: "status")
            <*> (v .: "title")
            <*> (v .:? "space")
            <*> (v .: "body")
            <*> (v .: "_expandable")
            <*> (v .: "_links")

data ContentType
    = PageContent
    | BlogpostContent
    | AttachmentContent
    | ContentContent
    deriving (Eq, Show)

instance FromJSON ContentType where
    parseJSON = withText "ContentType" $ \case
        "page" -> pure PageContent
        "blogpost" -> pure BlogpostContent
        "attachment" -> pure AttachmentContent
        "content" -> pure ContentContent
        t -> fail $ "Invalid ContentType '" <> T.unpack t <> "'"

instance ToJSON ContentType where
    toJSON PageContent = "page"
    toJSON BlogpostContent = "blogpost"
    toJSON AttachmentContent = "attachment"
    toJSON ContentContent = "content"

data ContentStatus
    = CurrentStatus
    | DeletedStatus
    | HistoricalStatus
    | DraftStatus
    deriving (Eq, Show)

instance FromJSON ContentStatus where
    parseJSON = withText "ContentStatus" $ \case
        "current" -> pure CurrentStatus
        "deleted" -> pure DeletedStatus
        "historical" -> pure HistoricalStatus
        "draft" -> pure DraftStatus
        t -> fail $ "Invalid ContentStatus '" <> T.unpack t <> "'"

-- Leaving out "_expandable" field as not needed yet.
data ContentBodyContainer = ContentBodyContainer
    { view :: ContentBody
    , exportView :: ContentBody
    , styledView :: ContentBody
    , storage :: ContentBody
    , wiki :: ContentBody
    , editor :: ContentBody
    , editor2 :: ContentBody
    , anonExportView :: ContentBody
    , atlasDocFmt :: ContentBody
    , dynamic :: ContentBody
    , _expandable :: Object
    }
    deriving (Generic, Show)

instance FromJSON ContentBodyContainer

-- Leaving out: webresource, mediatoken, embeddedContent, _expandable,
-- _genericLinks
data ContentBody = ContentBody
    { value :: Text
    , representation :: ContentRepresentation
    }
    deriving (Generic, Show)

instance FromJSON ContentBody

data ContentRepresentation
    = ViewRepresentation
    | ExportViewRepresentation
    | StyledViewRepresentation
    | StorageRepresentation
    | EditorRepresentation
    | Editor2Representation
    | AnonymousExportViewRepresentation
    | WikiRepresentation
    | AtlasDocFormatRepresentation
    deriving (Eq, Show)

instance FromJSON ContentRepresentation where
    parseJSON = withText "ContentRepresentation" $ \case
        "view" -> pure ViewRepresentation
        "export_view" -> pure ExportViewRepresentation
        "styled_view" -> pure StyledViewRepresentation
        "storage" -> pure StorageRepresentation
        "editor" -> pure EditorRepresentation
        "editor2" -> pure Editor2Representation
        "anonymous_export_view" -> pure AnonymousExportViewRepresentation
        "wiki" -> pure WikiRepresentation
        "atlas_doc_format" -> pure AtlasDocFormatRepresentation
        t -> fail $ "Invalid ContentRepresentation '" <> T.unpack t <> "'"

--------------------------------------------------------------------------------
