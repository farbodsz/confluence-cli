--------------------------------------------------------------------------------

module Confluence.API.Types.Result (
    ResultArray (..),
    ApiError (..),
) where

import Confluence.API.Types.Common (GenericLinks)
import Confluence.API.Types.Util (genericParseJSONWithRename)
import Data.Aeson (FromJSON (..), Object)
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | A list of result objects as returned by the Confluence API.
data ResultArray a = ResultArray
    { results :: [a]
    , start :: Int
    , limit :: Int
    , size :: Int
    , _links :: GenericLinks
    }
    deriving (Generic, Show)

instance FromJSON a => FromJSON (ResultArray a)

--------------------------------------------------------------------------------

-- | HTTP error (non-2xx status) as returned by the Confluence API.
data ApiError = ApiError
    { statusCode :: Int
    , _data :: Object
    , message :: Text
    , reason :: Text
    }
    deriving (Generic, Eq, Show)

instance FromJSON ApiError where
    parseJSON = genericParseJSONWithRename "_data" "data"

--------------------------------------------------------------------------------
