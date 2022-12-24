--------------------------------------------------------------------------------

module Confluence.Output.Format (
    -- * Table formats
    TableFormat (..),

    -- * Constructing tables
    table,
    record,
) where

import Confluence.Output.Table (
    Alignment (..),
    ColMeta (ColMeta),
    Column (Column),
    Table (..),
    TableSeparators (..),
    Width (Expandable),
 )
import Confluence.TextConversions (FromText (..), ToText (..))
import Data.Text (Text)
import Data.Tuple.Extra (uncurry3)

--------------------------------------------------------------------------------

data TableFormat = Plain | Pretty
    deriving (Eq)

instance FromText TableFormat where
    fromText "plain" = Just Plain
    fromText "pretty" = Just Pretty
    fromText _ = Nothing

instance ToText TableFormat where
    toText Plain = "plain"
    toText Pretty = "pretty"

table :: TableFormat -> [Text] -> [[Text]] -> Table
table fmt hs rows =
    Table
        { separators = tableSeparators fmt
        , columns = uncurry3 Column <$> zip3 metas headers rows
        }
  where
    metas = replicate (length headers + length rows) colMeta
    headers = Just <$> hs
    colMeta = ColMeta Expandable AlignLeft

record :: TableFormat -> [Text] -> [Text] -> Table
record fmt fields values =
    Table
        { separators = tableSeparators fmt
        , columns = [col fields, col values]
        }
  where
    col = Column colMeta Nothing
    colMeta = ColMeta Expandable AlignLeft

--------------------------------------------------------------------------------
-- Table separators

tableSeparators :: TableFormat -> TableSeparators
tableSeparators Plain = plainTableSeparators
tableSeparators Pretty = prettyTableSeparators

plainTableSeparators :: TableSeparators
plainTableSeparators = TableSeparators {colSep = "  "}

prettyTableSeparators :: TableSeparators
prettyTableSeparators = TableSeparators {colSep = " | "}

--------------------------------------------------------------------------------
