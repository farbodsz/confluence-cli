--------------------------------------------------------------------------------

module Confluence.API.Request (
    queryApi,
) where

import Confluence.Config (Config (..))
import Confluence.Error (ResponseError (..))
import Confluence.Monad (ConfluenceM)
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (
    FromJSON,
    decode,
 )
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
    getResponseStatusCode,
    httpLBS,
    setRequestBearerAuth,
    setRequestHost,
    setRequestMethod,
    setRequestPath,
    setRequestQueryString,
 )
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Request

-- | Request with headers and host set based on user configuration.
baseRequest :: Config -> Request
baseRequest cfg =
    setRequestMethod "GET" $
        setRequestBearerAuth (TE.encodeUtf8 cfg.apiToken) $
            setRequestHost (TE.encodeUtf8 cfg.url) defaultRequest

type Endpoint = String

mkRequest :: Config -> Endpoint -> Query -> Request
mkRequest cfg path query =
    setRequestPath (TE.encodeUtf8 fullPath) $
        setRequestQueryString query $
            baseRequest cfg
  where
    fullPath = T.pack $ "/rest/api" </> path

--------------------------------------------------------------------------------
-- Response

parseResponse :: FromJSON a => Response LB.ByteString -> Either ResponseError a
parseResponse resp = case getResponseStatusCode resp of
    200 -> maybeToEither ResponseDecodeError . decode . getResponseBody $ resp
    code -> Left $ HttpError code

queryApi :: FromJSON a => Endpoint -> Query -> ConfluenceM a
queryApi path query = do
    cfg <- ask
    resp <- liftIO $ httpLBS (mkRequest cfg path query)
    liftEither $ parseResponse resp

--------------------------------------------------------------------------------
