--------------------------------------------------------------------------------

module Confluence.Types.Space (
    Space (..),
    SpaceKey,
    SpaceType (..),
    SpaceArray,
) where

import Confluence.TextConversions
import Confluence.Types.Result (ResultArray)
import Confluence.Types.Util qualified as Util
import Data.Aeson (FromJSON (parseJSON), withText)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Types.QueryLike (QueryValueLike (toQueryValue))

--------------------------------------------------------------------------------

type SpaceArray = ResultArray Space

--------------------------------------------------------------------------------

data Space = Space
    { id :: Int
    , key :: SpaceKey
    , name :: Text
    , spaceType :: SpaceType
    , status :: Text
    }
    deriving (Generic, Show)

instance FromJSON Space where
    parseJSON = Util.genericParseJSONWithRename "spaceType" "type"

type SpaceKey = Text

data SpaceType = GlobalSpace | PersonalSpace
    deriving (Eq, Read, Show)

instance FromText SpaceType where
    fromText "global" = Just GlobalSpace
    fromText "personal" = Just PersonalSpace
    fromText _ = Nothing

instance ToText SpaceType where
    toText GlobalSpace = "global"
    toText PersonalSpace = "personal"

instance FromJSON SpaceType where
    parseJSON = withText "SpaceType" $ \case
        "global" -> pure GlobalSpace
        "personal" -> pure PersonalSpace
        _ -> fail "Invalid SpaceType"

instance QueryValueLike SpaceType where
    toQueryValue = Just . T.encodeUtf8 . toText

--------------------------------------------------------------------------------
