--------------------------------------------------------------------------------

module Confluence.Types.Space (
    Space (..),
    SpaceKey,
    SpaceType (..),
    SpaceArray,
) where

import Confluence.Display
import Confluence.Types.Common
import Confluence.Types.ResultArray (ResultArray)
import Data.Aeson (
    FromJSON (parseJSON),
    Object,
    withObject,
    withText,
    (.:),
 )
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Types.QueryLike (QueryValueLike (toQueryValue))

--------------------------------------------------------------------------------

type SpaceArray = ResultArray Space

--------------------------------------------------------------------------------

-- We leave out these fields from Space, as we don't care about them:
--
--   * description
--   * icon
--   * homepage
--   * metadata
--   * operations
--   * permissions
--   * settings
--   * theme
--   * lookAndFeel
--   * history
--
data Space = Space
    { id :: Int
    , key :: SpaceKey
    , name :: Text
    , spaceType :: SpaceType
    , _links :: GenericLinks
    , _expandable :: Object
    }
    deriving (Show)

instance FromJSON Space where
    parseJSON = withObject "Space" $ \v ->
        Space
            <$> (v .: "id")
            <*> (v .: "key")
            <*> (v .: "name")
            <*> (v .: "type")
            <*> (v .: "_links")
            <*> (v .: "_expandable")

type SpaceKey = Text

data SpaceType = GlobalSpace | PersonalSpace
    deriving (Eq, Read, Show)

instance Display SpaceType where
    display GlobalSpace = "global"
    display PersonalSpace = "personal"

instance FromJSON SpaceType where
    parseJSON = withText "SpaceType" $ \case
        "global" -> pure GlobalSpace
        "personal" -> pure PersonalSpace
        _ -> fail "Invalid SpaceType"

instance QueryValueLike SpaceType where
    toQueryValue GlobalSpace = Just "global"
    toQueryValue PersonalSpace = Just "personal"

data SpaceDescriptions = SpaceDescriptions
    { plain :: SpaceDescription
    , view :: SpaceDescription
    , _expandable :: DescriptionExpandable
    }
    deriving (Generic, Show)

instance FromJSON SpaceDescriptions

data SpaceDescription = SpaceDescription
    { value :: Text
    , representation :: SpaceRepresentation
    , embeddedContent :: [Object]
    }
    deriving (Generic, Show)

instance FromJSON SpaceDescription

data SpaceRepresentation = PlainRepresentation | ViewRepresentation
    deriving (Eq, Show)

instance FromJSON SpaceRepresentation where
    parseJSON = withText "SpaceRepresentation" $ \case
        "plain" -> pure PlainRepresentation
        "view" -> pure ViewRepresentation
        t -> fail $ "Invalid representation '" <> T.unpack t <> "'"

data DescriptionExpandable = DescriptionExpandable
    { view :: Text
    , plain :: Text
    }
    deriving (Generic, Show)

instance FromJSON DescriptionExpandable

--------------------------------------------------------------------------------
