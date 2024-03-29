--------------------------------------------------------------------------------

module Confluence.API.Types.Content (
    -- * Core content types
    Content (..),
    ContentId,
    ContentType (..),
    ContentStatus (..),

    -- * Content body and representation
    ContentBodyContainer (..),
    ContentBody (..),
    ContentRepresentation (..),

    -- * Content chidlren
    ContentChildren (..),

    -- * Misc
    ContentArray,
) where

import Confluence.API.Types.Common (GenericLinks)
import Confluence.API.Types.Result (ResultArray)
import Confluence.API.Types.Space (Space)
import Confluence.API.Types.Util qualified as Util
import Confluence.API.Types.Version (Version)
import Confluence.TextConversions
import Data.Aeson (
    FromJSON (parseJSON),
    Object,
    Options (..),
    ToJSON (toJSON),
    defaultOptions,
    genericParseJSON,
 )
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

type ContentArray = ResultArray Content

--------------------------------------------------------------------------------

type ContentId = Text

-- We leave out these fields from Content, as we don't care about them for now:
--
-- Note: some of these depend on what "expandable" properties are set in the get
-- call.
--
--   * history
--   * ancestors
--   * operations
--   * descendants
--   * container
--   * restrictions
--   * metadata
--   * macroRenderedOutput
--   * extensions
--
data Content = Content
    { id :: ContentId
    , contentType :: ContentType
    , status :: ContentStatus
    , title :: Text
    , space :: Space
    , version :: Version
    , body :: ContentBodyContainer
    , _expandable :: Object
    , _links :: GenericLinks
    }
    deriving (Generic, Show)

instance FromJSON Content where
    parseJSON = Util.genericParseJSONWithRename [("contentType", "type")]

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

instance ToText ContentType where
    toText PageContent = "page"
    toText BlogpostContent = "blogpost"
    toText AttachmentContent = "attachment"
    toText ContentContent = "content"

instance ToJSON ContentType where
    toJSON = toJSONViaText

data ContentStatus
    = CurrentStatus
    | DeletedStatus
    | TrashedStatus
    | HistoricalStatus
    | DraftStatus
    deriving (Eq, Show)

instance FromText ContentStatus where
    fromText "current" = Just CurrentStatus
    fromText "deleted" = Just DeletedStatus
    fromText "trashed" = Just TrashedStatus
    fromText "historical" = Just HistoricalStatus
    fromText "draft" = Just DraftStatus
    fromText _ = Nothing

instance FromJSON ContentStatus where
    parseJSON = parseJSONViaText "ContentStatus"

instance ToText ContentStatus where
    toText CurrentStatus = "current"
    toText DeletedStatus = "deleted"
    toText TrashedStatus = "trashed"
    toText HistoricalStatus = "historical"
    toText DraftStatus = "draft"

instance ToJSON ContentStatus where
    toJSON = toJSONViaText

--------------------------------------------------------------------------------

-- Leaving out "_expandable" field as not needed yet.
data ContentBodyContainer = ContentBodyContainer
    { view :: Maybe ContentBody
    , exportView :: Maybe ContentBody
    , styledView :: Maybe ContentBody
    , storage :: Maybe ContentBody
    , wiki :: Maybe ContentBody
    , editor :: Maybe ContentBody
    , editor2 :: Maybe ContentBody
    , anonExportView :: Maybe ContentBody
    , atlasDocFmt :: Maybe ContentBody
    , dynamic :: Maybe ContentBody
    , _expandable :: Object
    }
    deriving (Generic, Show)

instance FromJSON ContentBodyContainer where
    parseJSON =
        genericParseJSON $
            defaultOptions {fieldLabelModifier = Util.snakeToCamel}

-- Leaving out: webresource, mediatoken, embeddedContent, _expandable,
-- _genericLinks
data ContentBody = ContentBody
    { value :: Text
    , representation :: ContentRepresentation
    }
    deriving (Generic, Show)

instance FromJSON ContentBody
instance ToJSON ContentBody

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

data ContentChildren = ContentChildren
    { attachment :: ContentArray
    , comment :: ContentArray
    , page :: ContentArray
    }
    deriving (Generic, Show)

instance FromJSON ContentChildren

--------------------------------------------------------------------------------
