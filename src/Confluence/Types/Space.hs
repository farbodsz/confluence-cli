--------------------------------------------------------------------------------

module Confluence.Types.Space
    ( Space(..)
    , SpaceArray(..)
    ) where

import           Confluence.Types.Common
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , Object
                                                , withObject
                                                , withText
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

--------------------------------------------------------------------------------

-- We leave out these fields from Space, as we don't care about them:
--
--   * description
--   * icon 
--   * homepage 
--   * metadata
--   * operations
--   * permissions
--   * settings
--   * theme
--   * lookAndFeel
--   * history
--
data Space = Space
    { spId         :: Int
    , spKey        :: Text
    , spName       :: Text
    , spType       :: SpaceType
    , spLinks      :: GenericLinks
    , spExpandable :: Object
    }
    deriving Show

instance FromJSON Space where
    parseJSON = withObject "Space" $ \v ->
        Space
            <$> (v .: "id")
            <*> (v .: "key")
            <*> (v .: "name")
            <*> (v .: "type")
            <*> (v .: "_links")
            <*> (v .: "_expandable")


data SpaceType = GlobalSpace | PersonalSpace
    deriving Show

instance FromJSON SpaceType where
    parseJSON = withText "SpaceType" $ \case
        "global"   -> pure GlobalSpace
        "personal" -> pure PersonalSpace
        _          -> fail "Invalid SpaceType"



data SpaceDescriptions = SpaceDescriptions
    { spdsPlain      :: SpaceDescription
    , spdsView       :: SpaceDescription
    , spdsExpandable :: DescriptionExpandable
    }
    deriving (Generic, Show)

instance FromJSON SpaceDescriptions where
    parseJSON = withObject "SpaceDescriptions" $ \v ->
        SpaceDescriptions
            <$> (v .: "plain")
            <*> (v .: "view")
            <*> (v .: "_expandable")


data SpaceDescription = SpaceDescription
    { spdValue           :: Text
    , spdRepresentation  :: Representation
    , spdEmbeddedContent :: [Object]
    }
    deriving (Generic, Show)

instance FromJSON SpaceDescription where
    parseJSON = withObject "SpaceDescriptions" $ \v ->
        SpaceDescription
            <$> (v .: "value")
            <*> (v .: "representation")
            <*> (v .: "embeddedContent")


data DescriptionExpandable = DescriptionExpandable
    { dexpView  :: Text
    , dexpPlain :: Text
    }
    deriving (Generic, Show)

instance FromJSON DescriptionExpandable where
    parseJSON = withObject "DescriptionExpandable"
        $ \v -> DescriptionExpandable <$> (v .: "view") <*> (v .: "plain")

--------------------------------------------------------------------------------

data SpaceArray = SpaceArray
    { sparrResults :: [Space]
    , sparrStart   :: Int
    , sparrLimit   :: Int
    , sparrSize    :: Int
    , sparrLinks   :: GenericLinks
    }
    deriving (Generic, Show)

instance FromJSON SpaceArray where
    parseJSON = withObject "SpaceArray" $ \v ->
        SpaceArray
            <$> (v .: "results")
            <*> (v .: "start")
            <*> (v .: "limit")
            <*> (v .: "size")
            <*> (v .: "_links")

--------------------------------------------------------------------------------
