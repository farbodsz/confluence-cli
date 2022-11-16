--------------------------------------------------------------------------------

-- | Things that can be pretty printed
module Confluence.Display (
    Display (..),
) where

import Data.Text qualified as T

--------------------------------------------------------------------------------

class Display a where
    display :: a -> T.Text

instance Display T.Text where
    display = id

instance Display Int where
    display = T.pack . show

--------------------------------------------------------------------------------
