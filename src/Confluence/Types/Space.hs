--------------------------------------------------------------------------------

module Confluence.Types.Space (
    Space (..),
    SpaceType (..),
    SpaceArray (..),
) where

import Confluence.Display
import Confluence.Types.Common
import Data.Aeson (
    FromJSON (parseJSON),
    Object,
    withObject,
    withText,
    (.:),
 )
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types.QueryLike (QueryValueLike (toQueryValue))

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
    , key :: Text
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
    , representation :: Representation
    , embeddedContent :: [Object]
    }
    deriving (Generic, Show)

instance FromJSON SpaceDescription

data DescriptionExpandable = DescriptionExpandable
    { view :: Text
    , plain :: Text
    }
    deriving (Generic, Show)

instance FromJSON DescriptionExpandable

--------------------------------------------------------------------------------

data SpaceArray = SpaceArray
    { results :: [Space]
    , start :: Int
    , limit :: Int
    , size :: Int
    , _links :: GenericLinks
    }
    deriving (Generic, Show)

instance FromJSON SpaceArray

--------------------------------------------------------------------------------
