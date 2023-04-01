--------------------------------------------------------------------------------

module Confluence.API.Types.Search (
    -- * Confluence Query Language
    Cql (..),

    -- * Search result types
    SearchPageResponseSearchResult (..),
    SearchResult (..),
    ContainerSummary (..),
    Breadcrumb (..),
) where

import Confluence.API.Types.Common (GenericLinks)
import Confluence.API.Types.Content (Content)
import Confluence.API.Types.Space (Space)
import Confluence.API.Types.User (User)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Confluence Query Language.
newtype Cql = Cql {unCql :: Text}
    deriving (Eq, Show)

--------------------------------------------------------------------------------

data SearchPageResponseSearchResult = SearchPageResponseSearchResult
    { results :: [SearchResult]
    , start :: Int
    , limit :: Int
    , size :: Int
    , totalSize :: Int
    , cqlQuery :: Text
    , searchDuration :: Int
    , archivedResultCount :: Int
    , _links :: GenericLinks
    }
    deriving (Generic, Show)

instance FromJSON SearchPageResponseSearchResult

data SearchResult = SearchResult
    { content :: Maybe Content
    , user :: Maybe User
    , space :: Maybe Space
    , title :: Text
    , excerpt :: Text
    , url :: Text
    , resultParentContainer :: ContainerSummary
    , resultGlobalContainer :: ContainerSummary
    , breadcrumbs :: [Breadcrumb]
    , entityType :: Text
    , iconCssClass :: Text
    , lastModified :: Text -- TODO
    , friendlyLastModified :: Text
    , score :: Int
    }
    deriving (Generic, Show)

instance FromJSON SearchResult

data ContainerSummary = ContainerSummary
    { title :: Text
    , displayUrl :: Text
    }
    deriving (Generic, Show)

instance FromJSON ContainerSummary

data Breadcrumb = Breadcrumb
    { label :: Text
    , url :: Text
    , separator :: Text
    }
    deriving (Generic, Show)

instance FromJSON Breadcrumb

--------------------------------------------------------------------------------
