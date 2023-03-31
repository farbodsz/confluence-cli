--------------------------------------------------------------------------------

-- | CLI actions.
module Confluence.CLI (
    -- * Pages
    module Confluence.CLI.Page,

    -- * Spaces
    module Confluence.CLI.Space,

    -- * Utilites and types
    ContentIdentification (..),
) where

import Confluence.CLI.Page
import Confluence.CLI.Space
import Confluence.CLI.Types (ContentIdentification (..))

--------------------------------------------------------------------------------
