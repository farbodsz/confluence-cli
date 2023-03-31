--------------------------------------------------------------------------------

module Confluence.API.Types.User (
    User (..),
    UserType (..),
) where

import Confluence.API.Types.Common (GenericLinks)
import Confluence.API.Types.Util qualified as Util
import Confluence.TextConversions (FromText (..), parseJSONViaText)
import Data.Aeson (FromJSON (parseJSON), Object)
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- Note: missing out these fields
--
--   * userKey
--   * accountId
--   * accountType
--   * email
--   * publicName
--   * profilePicture
--   * timeZone
--   * isExternalCollaborator
--   * externalCollaborator
--   * operations
--   * details
--   * personalSpace
--
data User = User
    { userType :: UserType
    , -- Atlassian says this is no longer available / deprecated and to use
      -- @accountId@ instead, but JSON responses have @username@ and not
      -- @accountId@.
      username :: Text
    , displayName :: Text
    , _expandable :: Object
    , _links :: GenericLinks
    }
    deriving (Generic, Show)

instance FromJSON User where
    parseJSON = Util.genericParseJSONWithRename "userType" "type"

data UserType
    = KnownUser
    | UnknownUser
    | AnonymousUser
    | UserUser
    deriving (Show)

instance FromText UserType where
    fromText "known" = Just KnownUser
    fromText "unknown" = Just UnknownUser
    fromText "anonymous" = Just AnonymousUser
    fromText "user" = Just UserUser
    fromText _ = Nothing

instance FromJSON UserType where
    parseJSON = parseJSONViaText "UserType"

--------------------------------------------------------------------------------
