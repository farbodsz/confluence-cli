--------------------------------------------------------------------------------

-- | Search-related actions.
module Confluence.CLI.Search (
    search,
) where

import Confluence.API.Endpoints qualified as API
import Confluence.API.Types
import Confluence.CLI.Types (SearchScope (..))
import Confluence.CLI.Util
import Confluence.Config (Config)
import Confluence.Monad (runConfluence)

--------------------------------------------------------------------------------

search :: Config -> Cql -> SearchScope -> Int -> Int -> IO ()
search cfg cql scope start limit = do
    result <- runConfluence cfg $ searchFunction cql start limit
    withEither result $ \searchResult -> print searchResult.results
  where
    searchFunction = case scope of
        SearchContent -> API.searchContent
        SearchUsers -> API.searchUsers

--------------------------------------------------------------------------------
