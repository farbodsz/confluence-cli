--------------------------------------------------------------------------------

module Confluence.API where

import           Confluence.Config              ( loadConfig )
import qualified Data.ByteString.Lazy.Char8    as L8
import           Network.HTTP.Simple            ( getResponseBody
                                                , getResponseHeader
                                                , getResponseStatusCode
                                                , httpLBS
                                                )

--------------------------------------------------------------------------------

testApi :: IO ()
testApi = do
    cfg <- loadConfig
    print cfg

    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "Status code was: " ++ show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response

--------------------------------------------------------------------------------
