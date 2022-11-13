----------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}

module Confluence.Table
    ( ColMeta(..)
    , Width(..)
    , Alignment(..)
    , table
    , printTable
    , defaultColMeta
    , defaultColSep
    , defaultTable
    ) where

import           Confluence.Display
import           Data.List                      ( transpose )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

--------------------------------------------------------------------------------
-- Column meta

data Width = Fixed Int | Expandable

data Alignment = AlignLeft | AlignCenter | AlignRight

data ColMeta = ColMeta
    { cmWidth     :: Width
    , cmAlignment :: Alignment
    }

--------------------------------------------------------------------------------
-- Cell

-- | @pad alignment width text@ returns a @text@ surrounded by space characters
-- such that it has a length of @width@.
pad :: Alignment -> Int -> T.Text -> T.Text
pad AlignLeft   n = T.justifyLeft n ' '
pad AlignCenter n = T.center n ' '
pad AlignRight  n = T.justifyRight n ' '

--------------------------------------------------------------------------------
-- Table

data Table = Table
    { tableColSep :: T.Text
    , tableCols   :: [Column]
    }

data Column where
  Column :: Display a => { colMeta :: ColMeta, colData :: [a]} -> Column

table :: Display a => T.Text -> [ColMeta] -> [[a]] -> Table
table colSep metas xss = Table colSep $ uncurry Column <$> zip metas xss

-- | @renderTable table@ produces a list of columns where each column is a list
-- of texts.
renderTable :: Table -> [T.Text]
renderTable (Table colSep cols) = T.intercalate colSep <$> transpose colTexts
  where
    colTexts = mkCol <$> cols
    mkCol (Column meta xs) = pad (cmAlignment meta) w <$> ts
      where
        w = case cmWidth meta of
            Fixed x    -> x
            Expandable -> maximum (T.length <$> ts)
        ts = display <$> xs

printTable :: Table -> IO ()
printTable = mapM_ T.putStrLn . renderTable

----------------------------------------------------------------------------------
-- Defaults

defaultColMeta :: ColMeta
defaultColMeta = ColMeta Expandable AlignLeft

defaultColSep :: T.Text
defaultColSep = "  "

-- | Table with default column metas and default column separator.
defaultTable :: Display a => [[a]] -> Table
defaultTable xss = table defaultColSep metas xss
    where metas = replicate (length xss) defaultColMeta

-- -- | Table with default column metas and default column separator.
-- defaultTable :: Displayable a => [a] -> [[a]] -> Table
-- defaultTable colNames xss = table defaultColSep metas cols
--   where
--     cols  = uncurry (:) <$> zip colNames xss
--     metas = replicate (length xss) defaultColMeta

----------------------------------------------------------------------------------
