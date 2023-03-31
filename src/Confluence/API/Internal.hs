--------------------------------------------------------------------------------

module Confluence.API.Internal (
    getApi,
    deleteApi,
    postApi,
    putApi,
) where

import Confluence.Config (Config (..))
import Confluence.Error (ResponseError (..))
import Confluence.Monad (ConfluenceM)
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Lazy qualified as LB
import Data.Either.Extra (maybeToEither)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Simple (
    Query,
    Request,
    Response,
    defaultRequest,
    getResponseBody,
    getResponseStatus,
    httpLBS,
    setRequestBearerAuth,
    setRequestBodyJSON,
    setRequestHost,
    setRequestMethod,
    setRequestPath,
    setRequestQueryString,
 )
import Network.HTTP.Types (statusIsSuccessful)
import System.FilePath ((</>))

type Endpoint = String

-- | Creates a request with headers and host set based on user configuration.
baseRequest :: Config -> Endpoint -> Request
baseRequest cfg path =
    setRequestBearerAuth (TE.encodeUtf8 cfg.token) $
        setRequestHost (TE.encodeUtf8 cfg.url) $
            setRequestPath (TE.encodeUtf8 fullPath) defaultRequest
  where
    fullPath = T.pack $ "/rest/api" </> path

-- | @parseResponse response@ returns a @(status, responseObject)@ tuple, by
-- attempts to parse the HTTP response bytestring into a JSON type.
parseResponse :: FromJSON a => Response LB.ByteString -> Either ResponseError a
parseResponse resp
    | statusIsSuccessful status = decodeRespEither body
    | otherwise = Left $ HttpError status (decode body)
  where
    body = getResponseBody resp
    status = getResponseStatus resp
    decodeRespEither r = maybeToEither (ResponseDecodeError r) . decode $ r

-- | Helper function which makes the HTTP request, processes, and returns its
-- response.
doRequest :: FromJSON a => Request -> ConfluenceM a
doRequest req = do
    resp <- liftIO $ httpLBS req
    liftEither $ parseResponse resp

--------------------------------------------------------------------------------

getApi :: FromJSON a => Endpoint -> Query -> ConfluenceM a
getApi path query = do
    cfg <- ask
    doRequest $
        setRequestMethod "GET" $
            setRequestQueryString query $
                baseRequest cfg path

deleteApi :: FromJSON a => Endpoint -> Query -> ConfluenceM a
deleteApi path query = do
    cfg <- ask
    doRequest $
        setRequestMethod "DELETE" $
            setRequestQueryString query $
                baseRequest cfg path

postApi :: (FromJSON a, ToJSON b) => Endpoint -> b -> ConfluenceM a
postApi path payload = do
    cfg <- ask
    doRequest $
        setRequestMethod "POST" $
            setRequestBodyJSON payload $
                baseRequest cfg path

putApi :: (FromJSON a, ToJSON b) => Endpoint -> b -> ConfluenceM a
putApi path payload = do
    cfg <- ask
    doRequest $
        setRequestMethod "PUT" $
            setRequestBodyJSON payload $
                baseRequest cfg path

--------------------------------------------------------------------------------
