--------------------------------------------------------------------------------

module Confluence.Error (
    ResponseError (..),
    errorMsg,
) where

import Data.Text qualified as T

--------------------------------------------------------------------------------

data ResponseError
    = HttpError Int
    | ResponseDecodeError
    deriving (Eq, Show)

errorMsg :: ResponseError -> T.Text
errorMsg = T.pack . show -- TODO:

--------------------------------------------------------------------------------
