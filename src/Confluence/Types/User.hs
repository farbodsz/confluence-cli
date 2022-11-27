--------------------------------------------------------------------------------

module Confluence.Types.User (
    User (..),
    UserType (..),
) where

import Confluence.TextConversions (FromText (..), parseJSONViaText)
import Confluence.Types.Common (GenericLinks)
import Data.Aeson (FromJSON (parseJSON), Object, withObject, (.:))
import Data.Text (Text)

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
    deriving (Show)

instance FromJSON User where
    parseJSON = withObject "User" $ \v ->
        User
            <$> (v .: "type")
            <*> (v .: "username")
            <*> (v .: "displayName")
            <*> (v .: "_expandable")
            <*> (v .: "_links")

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
