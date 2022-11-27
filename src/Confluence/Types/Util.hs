--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

-- | Internal utility functions.
module Confluence.Types.Util (genericParseJSONWithRename) where

import Data.Aeson (Options (..), Value, Zero, defaultOptions, genericParseJSON)
import Data.Aeson.Types (GFromJSON, Parser)
import GHC.Generics (Generic, Rep)

--------------------------------------------------------------------------------

genericParseJSONWithRename ::
    (Generic a, GFromJSON Zero (Rep a)) =>
    String ->
    String ->
    Value ->
    Parser a
genericParseJSONWithRename old new =
    genericParseJSON $ defaultOptions {fieldLabelModifier = renameField old new}

renameField :: String -> String -> String -> String
renameField old new x
    | old == x = new
    | otherwise = x

--------------------------------------------------------------------------------
