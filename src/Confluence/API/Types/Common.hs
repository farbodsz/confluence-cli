--------------------------------------------------------------------------------

module Confluence.API.Types.Common (GenericLinks) where

import Data.Map qualified as M
import Data.Text (Text)

--------------------------------------------------------------------------------

type LinkType = Text
type Url = Text

type GenericLinks = M.Map LinkType Url

--------------------------------------------------------------------------------
