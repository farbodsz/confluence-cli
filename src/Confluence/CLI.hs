--------------------------------------------------------------------------------

-- | CLI actions.
module Confluence.CLI (
    -- * Pages
    module Confluence.CLI.Page,

    -- * Search
    module Confluence.CLI.Search,

    -- * Spaces
    module Confluence.CLI.Space,

    -- * Utilites and types
    ContentIdentification (..),
) where

import Confluence.CLI.Page
import Confluence.CLI.Search
import Confluence.CLI.Space
import Confluence.CLI.Types (ContentIdentification (..))

--------------------------------------------------------------------------------
