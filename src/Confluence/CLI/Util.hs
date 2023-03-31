--------------------------------------------------------------------------------

-- | CLI action utilities.
module Confluence.CLI.Util (
    withEither,
    toTextF,
    confirm,
) where

import Confluence.Error (ResponseError, errorMsg)
import Confluence.TextConversions (ToText (toText))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.IO (hFlush, stdout)

--------------------------------------------------------------------------------

-- | @handleErr action@ prints the error message if there is one, else runs the
-- @action@.
withEither :: Either ResponseError a -> (a -> IO ()) -> IO ()
withEither e action = either (T.putStrLn . ("Error:  " <>) . errorMsg) action e

toTextF :: (Functor f, ToText a) => f a -> f T.Text
toTextF = fmap toText

confirm :: T.Text -> IO Bool
confirm msg = do
    T.putStr $ msg <> " [Y/n]: "
    hFlush stdout
    ans <- T.getLine
    pure $ T.toLower ans == "y"

--------------------------------------------------------------------------------
