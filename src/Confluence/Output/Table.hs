----------------------------------------------------------------------------------

module Confluence.Output.Table (
    -- * Table
    Table (..),
    printTable,

    -- * Options
    TableSeparators (..),
    Column (..),
    ColMeta (..),
    Width (..),
    Alignment (..),
) where

import Data.List (transpose)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

--------------------------------------------------------------------------------
-- Types

data Table = Table
    { separators :: TableSeparators
    , columns :: [Column]
    }
    deriving (Show)

data TableSeparators = TableSeparators
    { colSep :: Text
    }
    deriving (Show)

data Column = Column
    { meta :: ColMeta
    , header :: Maybe Text
    , rows :: [Text]
    }
    deriving (Show)

data ColMeta = ColMeta
    { width :: Width
    , alignment :: Alignment
    }
    deriving (Show)

data Width = Fixed Int | Expandable
    deriving (Show)

data Alignment = AlignLeft | AlignCenter | AlignRight
    deriving (Show)

--------------------------------------------------------------------------------
-- Rendering tables

-- | @renderTable table@ produces a list of rendered rows.
renderTable :: Table -> [Text]
renderTable (Table seps cols) = T.intercalate seps.colSep <$> transpose colTexts
  where
    colTexts = mkCol <$> cols

-- | @mkCol column@ renders a column as a list or rows.
mkCol :: Column -> [Text]
mkCol (Column meta mheader xs) = pad meta.alignment w <$> (headerRow <> xs)
  where
    headerRow = maybeToList mheader
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
