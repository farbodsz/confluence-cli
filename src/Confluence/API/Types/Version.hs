--------------------------------------------------------------------------------

module Confluence.API.Types.Version (Version (..)) where

import Confluence.API.Types.User (User)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Time (ZonedTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Version information about some Content.
--
-- Note: missing out these fields as they are not needed in our CLI
--
--   * friendlyWhen
--   * content
--   * collaborators
--   * _expandable
--   * _links
--   * contentTypeModified
--   * confRev
--   * syncRev
--   * syncRevSource
data Version = Version
    { by :: User
    , when :: ZonedTime
    , message :: Text
    , number :: Int
    , minorEdit :: Bool
    }
    deriving (Generic, Show)

instance FromJSON Version

--------------------------------------------------------------------------------
