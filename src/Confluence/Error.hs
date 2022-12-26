--------------------------------------------------------------------------------

module Confluence.Error (
    ResponseError (..),
    errorMsg,
) where

import Confluence.Types (ApiError (..))
import Data.ByteString.Lazy qualified as LB
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (Status)

--------------------------------------------------------------------------------

data ResponseError
    = -- | HTTP error response from the Confluence API.
      -- The API does not define schemas for error responses, so if unable to
      -- decode the response, then @ApiError@ will be @Nothing@.
      HttpError Status (Maybe ApiError)
    | ResponseDecodeError LB.ByteString
    deriving (Eq, Show)

errorMsg :: ResponseError -> T.Text
errorMsg (HttpError status mApiError) =
    "HTTP "
        <> T.pack (show status)
        <> maybe "" (\apiErr -> "\n" <> apiErr.message) mApiError
errorMsg (ResponseDecodeError content) =
    "ResponseDecodeError: Unable to decode response:\n" <> lbToText content

lbToText :: LB.ByteString -> T.Text
lbToText = TE.decodeUtf8 . LB.toStrict

--------------------------------------------------------------------------------
