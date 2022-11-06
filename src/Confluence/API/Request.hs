--------------------------------------------------------------------------------

module Confluence.API.Request
    ( handleApi
    , ResponseError(..)
    ) where

import           Confluence.Config              ( Config(..) )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Aeson                     ( FromJSON
                                                , decode
                                                )
import qualified Data.ByteString.Lazy          as LB
import           Data.Either.Extra              ( maybeToEither )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Network.HTTP.Simple            ( Query
                                                , Request
                                                , Response
                                                , defaultRequest
                                                , getResponseBody
                                                , getResponseStatusCode
                                                , httpLBS
                                                , setRequestBearerAuth
                                                , setRequestHost
                                                , setRequestMethod
                                                , setRequestPath
                                                , setRequestQueryString
                                                )
import           System.FilePath                ( (</>) )

--------------------------------------------------------------------------------
-- Request

-- | Request with headers and host set based on user configuration.
baseRequest :: Config -> Request
baseRequest Config {..} =
    setRequestMethod "GET"
        $ setRequestBearerAuth (TE.encodeUtf8 cfgApiToken)
        $ setRequestHost (TE.encodeUtf8 cfgUrl) defaultRequest

type Endpoint = String

mkRequest :: Config -> Endpoint -> Query -> Request
mkRequest cfg path query =
    setRequestPath (TE.encodeUtf8 fullPath)
        $ setRequestQueryString query
        $ baseRequest cfg
    where fullPath = T.pack $ "/rest/api" </> path

--------------------------------------------------------------------------------
-- Response

data ResponseError
    = HttpError Int
    | ResponseDecodeError
    deriving (Eq, Show)

parseResponse :: FromJSON a => Response LB.ByteString -> Either ResponseError a
parseResponse resp = case getResponseStatusCode resp of
    200  -> maybeToEither ResponseDecodeError . decode . getResponseBody $ resp
    code -> Left $ HttpError code

--------------------------------------------------------------------------------
-- Helper handler function

handleApi
    :: (MonadIO m, FromJSON a)
    => Config
    -> Endpoint
    -> Query
    -> m (Either ResponseError a)
handleApi cfg path query = parseResponse <$> httpLBS (mkRequest cfg path query)

--------------------------------------------------------------------------------
