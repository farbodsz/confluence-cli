--------------------------------------------------------------------------------

-- | Utility types used in the Cli.
module Confluence.CLI.Types (
    ContentIdentification (..),
) where

import Confluence.API.Types (ContentId, SpaceKey)
import Data.Text (Text)

--------------------------------------------------------------------------------

-- | Content on Confluence can be identified either by an integer ID, or by the
-- space key and content title.
data ContentIdentification = ContentId ContentId | ContentName SpaceKey Text
    deriving (Eq)

--------------------------------------------------------------------------------
