--------------------------------------------------------------------------------

module Confluence.Types.Common (
    GenericLinks,
    Representation,
) where

import Data.Aeson (
    FromJSON (parseJSON),
    Object,
    withText,
 )

--------------------------------------------------------------------------------

type GenericLinks = Object

data Representation = PlainRepresentation | ViewRepresentation
    deriving (Eq, Show)

instance FromJSON Representation where
    parseJSON = withText "Representation" $ \case
        "plain" -> pure PlainRepresentation
        "view" -> pure ViewRepresentation
        _ -> fail "Invalid representation"

--------------------------------------------------------------------------------
