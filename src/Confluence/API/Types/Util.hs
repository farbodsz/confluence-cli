--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

-- | Internal utility functions.
module Confluence.API.Types.Util (
    genericParseJSONWithRename,
    snakeToCamel,
) where

import Confluence.Util qualified as Util
import Data.Aeson (Options (..), Value, Zero, defaultOptions, genericParseJSON)
import Data.Aeson.Types (GFromJSON, Parser)
import Data.Char (toUpper)
import GHC.Generics (Generic, Rep)

--------------------------------------------------------------------------------

genericParseJSONWithRename ::
    (Generic a, GFromJSON Zero (Rep a)) =>
    [(String, String)] ->
    Value ->
    Parser a
genericParseJSONWithRename replacements =
    genericParseJSON $
        defaultOptions {fieldLabelModifier = Util.replaceAll replacements}

snakeToCamel :: String -> String
snakeToCamel [] = []
snakeToCamel (s : ss)
    | s == '_' = s : snakeToCamel ss
    | otherwise = go (s : ss)
  where
    go [] = []
    go [x] = [x]
    go (x1 : x2 : xs)
        | x1 == '_' = toUpper x2 : go xs
        | otherwise = x1 : go (x2 : xs)

--------------------------------------------------------------------------------
