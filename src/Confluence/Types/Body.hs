--------------------------------------------------------------------------------

module Confluence.Types.Body (BodyType (..)) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data BodyType = BodyType
    { representation :: Text
    , value :: Text
    }
    deriving (Generic, Show)

instance FromJSON BodyType

--------------------------------------------------------------------------------
