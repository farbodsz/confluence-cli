--------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances of library types.
module Confluence.Types.Instances where

import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Types.QueryLike

--------------------------------------------------------------------------------

instance QueryValueLike Int where
    toQueryValue = Just . B8.pack . show

--------------------------------------------------------------------------------
