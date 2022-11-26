--------------------------------------------------------------------------------

module Confluence.TextConversions (
    FromText (..),
    ToText (..),
    parseJSONViaText,
    toJSONViaText,
) where

import Data.Aeson.Types qualified as A
import Data.Text qualified as T

--------------------------------------------------------------------------------

class FromText a where
    fromText :: T.Text -> Maybe a

class ToText a where
    toText :: a -> T.Text

instance FromText T.Text where
    fromText = Just

instance ToText T.Text where
    toText = id

instance ToText Int where
    toText = T.pack . show

parseJSONViaText :: FromText a => String -> A.Value -> A.Parser a
parseJSONViaText ty_name = A.withText ty_name $ \t -> case fromText t of
    Nothing -> fail $ "Invalid " <> ty_name
    Just v -> pure v

toJSONViaText :: ToText a => a -> A.Value
toJSONViaText = A.toJSON . toText

--------------------------------------------------------------------------------
