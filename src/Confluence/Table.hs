----------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}

module Confluence.Table (
    ColMeta (..),
    Width (..),
    Alignment (..),
    table,
    printTable,
    defaultColMeta,
    defaultColSep,
    defaultTable,
) where

import Confluence.TextConversions (ToText (toText))
import Data.List (transpose)
import Data.Text qualified as T
import Data.Text.IO qualified as T

--------------------------------------------------------------------------------
-- Column meta

data Width = Fixed Int | Expandable

data Alignment = AlignLeft | AlignCenter | AlignRight

data ColMeta = ColMeta
    { width :: Width
    , alignment :: Alignment
    }

--------------------------------------------------------------------------------
-- Cell

-- | @pad alignment width text@ returns a @text@ surrounded by space characters
-- such that it has a length of @width@.
pad :: Alignment -> Int -> T.Text -> T.Text
pad AlignLeft n = T.justifyLeft n ' '
pad AlignCenter n = T.center n ' '
pad AlignRight n = T.justifyRight n ' '

--------------------------------------------------------------------------------
-- Table

data Table = Table
    { colSep :: T.Text
    , columns :: [Column]
    }

data Column where
    Column :: ToText a => {meta :: ColMeta, items :: [a]} -> Column

table :: ToText a => T.Text -> [ColMeta] -> [[a]] -> Table
table colSep metas xss = Table colSep $ uncurry Column <$> zip metas xss

-- | @renderTable table@ produces a list of columns where each column is a list
-- of texts.
renderTable :: Table -> [T.Text]
renderTable (Table colSep cols) = T.intercalate colSep <$> transpose colTexts
  where
    colTexts = mkCol <$> cols
    mkCol (Column meta xs) = pad meta.alignment w <$> ts
      where
        w = case meta.width of
            Fixed x -> x
            Expandable -> maximum (T.length <$> ts)
        ts = toText <$> xs

printTable :: Table -> IO ()
printTable = mapM_ T.putStrLn . renderTable

----------------------------------------------------------------------------------
-- Defaults

defaultColMeta :: ColMeta
defaultColMeta = ColMeta Expandable AlignLeft

defaultColSep :: T.Text
defaultColSep = "  "

-- | Table with default column metas and default column separator.
defaultTable :: ToText a => [[a]] -> Table
defaultTable xss = table defaultColSep metas xss
  where
    metas = replicate (length xss) defaultColMeta

----------------------------------------------------------------------------------
