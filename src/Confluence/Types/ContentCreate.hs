--------------------------------------------------------------------------------

module Confluence.Types.ContentCreate (
    ContentBodyCreate (..),
) where

import Confluence.Types.Page (ContentRepresentation)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data ContentBodyCreate = ContentBodyCreate
    { value :: Text
    , representation :: ContentRepresentation
    }
    deriving (Generic, Show)

instance ToJSON ContentBodyCreate

--------------------------------------------------------------------------------
