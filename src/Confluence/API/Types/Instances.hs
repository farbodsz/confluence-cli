--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances of library types.
module Confluence.API.Types.Instances where

import Data.ByteString.Char8 qualified as B8
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Types.QueryLike

--------------------------------------------------------------------------------

instance QueryValueLike Int where
    toQueryValue = Just . B8.pack . show

instance QueryValueLike [T.Text] where
    toQueryValue = Just . T.encodeUtf8 . T.intercalate ","

--------------------------------------------------------------------------------
