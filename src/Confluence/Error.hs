--------------------------------------------------------------------------------

module Confluence.Error (
    ResponseError (..),
    errorMsg,
) where

import Data.ByteString.Lazy qualified as LB
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

--------------------------------------------------------------------------------

data ResponseError
    = HttpError Int
    | ResponseDecodeError LB.ByteString
    deriving (Eq, Show)

errorMsg :: ResponseError -> T.Text
errorMsg (HttpError code) = "HTTP " <> T.pack (show code)
errorMsg (ResponseDecodeError content) =
    "ResponseDecodeError: Unable to decode response:\n"
        <> TE.decodeUtf8 (LB.toStrict content)

--------------------------------------------------------------------------------
