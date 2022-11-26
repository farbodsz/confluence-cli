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

import Confluence.TextConversions
import Confluence.Types.Common (GenericLinks)
import Confluence.Types.ResultArray (ResultArray)
import Confluence.Types.Space (Space)
import Data.Aeson (
    FromJSON (parseJSON),
    Object,
    ToJSON (toJSON),
    withObject,
    (.:),
 )
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

type ContentArray = ResultArray Content

--------------------------------------------------------------------------------

-- We leave out these fields from Content, as we don't care about them for now:
--
-- Note: some of these depend on what "expandable" properties are set in the get
-- call.
--
--   * body
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
--
data Content = Content
    { id :: Text
    , contentType :: ContentType
    , status :: ContentStatus
    , title :: Text
    , space :: Space
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
            <*> (v .: "space")
            <*> (v .: "_expandable")
            <*> (v .: "_links")

data ContentType
    = PageContent
    | BlogpostContent
    | AttachmentContent
    | ContentContent
    deriving (Eq, Show)

instance FromText ContentType where
    fromText "page" = Just PageContent
    fromText "blogpost" = Just BlogpostContent
    fromText "attachment" = Just AttachmentContent
    fromText "content" = Just ContentContent
    fromText _ = Nothing

instance FromJSON ContentType where
    parseJSON = parseJSONViaText "ContentType"

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

instance FromText ContentStatus where
    fromText "current" = Just CurrentStatus
    fromText "deleted" = Just DeletedStatus
    fromText "historical" = Just HistoricalStatus
    fromText "draft" = Just DraftStatus
    fromText _ = Nothing

instance FromJSON ContentStatus where
    parseJSON = parseJSONViaText "ContentStatus"

instance ToText ContentStatus where
    toText CurrentStatus = "current"
    toText DeletedStatus = "deleted"
    toText HistoricalStatus = "historical"
    toText DraftStatus = "draft"

instance ToJSON ContentStatus where
    toJSON = toJSONViaText

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
    deriving (Eq, Generic, Show)

instance FromText ContentRepresentation where
    fromText "view" = Just ViewRepresentation
    fromText "export_view" = Just ExportViewRepresentation
    fromText "styled_view" = Just StyledViewRepresentation
    fromText "storage" = Just StorageRepresentation
    fromText "editor" = Just EditorRepresentation
    fromText "editor2" = Just Editor2Representation
    fromText "anonymous_export_view" = Just AnonymousExportViewRepresentation
    fromText "wiki" = Just WikiRepresentation
    fromText "atlas_doc_format" = Just AtlasDocFormatRepresentation
    fromText _ = Nothing

instance FromJSON ContentRepresentation where
    parseJSON = parseJSONViaText "ContentRepresentation"

instance ToText ContentRepresentation where
    toText ViewRepresentation = "view"
    toText ExportViewRepresentation = "export_view"
    toText StyledViewRepresentation = "styled_view"
    toText StorageRepresentation = "storage"
    toText EditorRepresentation = "editor"
    toText Editor2Representation = "editor2"
    toText AnonymousExportViewRepresentation = "anonymous_export_view"
    toText WikiRepresentation = "wiki"
    toText AtlasDocFormatRepresentation = "atlas_doc_format"

instance ToJSON ContentRepresentation where
    toJSON = toJSONViaText

--------------------------------------------------------------------------------
