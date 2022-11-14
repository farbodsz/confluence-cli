--------------------------------------------------------------------------------

module Confluence.Types.ResultArray (ResultArray (..)) where

import Confluence.Types.Common (GenericLinks)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

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
