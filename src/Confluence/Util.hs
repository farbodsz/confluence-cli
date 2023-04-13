--------------------------------------------------------------------------------

module Confluence.Util (
    headMaybe,
    replaceAll,
) where

import Data.List.Extra (replace)

--------------------------------------------------------------------------------

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

replaceAll :: Eq a => [([a], [a])] -> [a] -> [a]
replaceAll xs target = foldl (flip $ uncurry replace) target xs

--------------------------------------------------------------------------------
