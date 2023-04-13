--------------------------------------------------------------------------------

module Confluence.API.Types.Common (
    -- * Types
    LinkType,
    Url,
    GenericLinks,

    -- * Util functions
    getWebLink,
) where

import Control.Monad (liftM2)
import Data.Map ((!?))
import Data.Map qualified as M
import Data.Text (Text)

--------------------------------------------------------------------------------

type LinkType = Text
type Url = Text

type GenericLinks = M.Map LinkType Url

--------------------------------------------------------------------------------

getWebLink :: GenericLinks -> Maybe Url
getWebLink links = liftM2 (<>) (links !? "base") (links !? "webui")

--------------------------------------------------------------------------------
