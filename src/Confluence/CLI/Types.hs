--------------------------------------------------------------------------------

-- | Utility types used in the Cli.
module Confluence.CLI.Types (
    ContentIdentification (..),
    SearchScope (..),
) where

import Confluence.API.Types (ContentId, SpaceKey)
import Confluence.TextConversions (FromText (..), ToText (..))
import Data.Text (Text)

--------------------------------------------------------------------------------

-- | Content on Confluence can be identified either by an integer ID, or by the
-- space key and content title.
data ContentIdentification = ContentId ContentId | ContentName SpaceKey Text
    deriving (Eq)

-- | Search can be performed on content and users.
data SearchScope = SearchContent | SearchUsers
    deriving (Eq)

instance FromText SearchScope where
    fromText "content" = Just SearchContent
    fromText "users" = Just SearchUsers
    fromText _ = Nothing

instance ToText SearchScope where
    toText SearchContent = "content"
    toText SearchUsers = "users"

--------------------------------------------------------------------------------
