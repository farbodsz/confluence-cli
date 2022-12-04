----------------------------------------------------------------------------------

module Confluence.Table (
    -- * Table
    table,
    printTable,

    -- * Defaults
    defaultTable,
    defaultColMeta,
    defaultColSep,
) where

import Data.List (transpose)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tuple.Extra (uncurry3)

--------------------------------------------------------------------------------
-- Types

data Table = Table
    { separators :: TableSeparators
    , columns :: [Column]
    }

type Separator = Text

data TableSeparators = TableSeparators {colSep :: Separator}

data Column = Column
    { meta :: ColMeta
    , header :: Text
    , rows :: [Text]
    }

data ColMeta = ColMeta
    { width :: Width
    , alignment :: Alignment
    }

data Width = Fixed Int | Expandable

data Alignment = AlignLeft | AlignCenter | AlignRight

--------------------------------------------------------------------------------
-- Constructing tables

-- | @table separator metas headers rows@ constructs a table with those options
table :: Separator -> [ColMeta] -> [Text] -> [[Text]] -> Table
table sep metas hs xss =
    Table
        { separators = TableSeparators sep
        , columns = uncurry3 Column <$> zip3 metas hs xss
        }

--------------------------------------------------------------------------------
-- Rendering tables

-- | @renderTable table@ produces a list of columns where each column is a list
-- of texts.
renderTable :: Table -> [Text]
renderTable (Table seps cols) = T.intercalate seps.colSep <$> transpose colTexts
  where
    colTexts = mkCol <$> cols

mkCol :: Column -> [Text]
mkCol (Column meta header xs) = pad meta.alignment w <$> (header : xs)
  where
    w = case meta.width of
        Fixed x -> x
        Expandable -> maximum (T.length <$> xs)

printTable :: Table -> IO ()
printTable = mapM_ T.putStrLn . renderTable

--------------------------------------------------------------------------------
-- Cell

-- | @pad alignment width text@ returns a @text@ surrounded by space characters
-- such that it has a length of @width@.
pad :: Alignment -> Int -> Text -> Text
pad AlignLeft n = T.justifyLeft n ' '
pad AlignCenter n = T.center n ' '
pad AlignRight n = T.justifyRight n ' '

----------------------------------------------------------------------------------
-- Defaults

-- | @defaultTable headers rows@ creates a table with default column metas and
-- default column separator.
defaultTable :: [Text] -> [[Text]] -> Table
defaultTable headers rows = table defaultColSep metas headers rows
  where
    metas = replicate (length headers + length rows) defaultColMeta

defaultColMeta :: ColMeta
defaultColMeta = ColMeta Expandable AlignLeft

defaultColSep :: Text
defaultColSep = "  "

----------------------------------------------------------------------------------
