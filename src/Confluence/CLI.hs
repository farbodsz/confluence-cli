module Confluence.CLI (someFunc) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple (
    getResponseBody,
    getResponseHeader,
    getResponseStatusCode,
    httpLBS,
 )

someFunc :: IO ()
someFunc = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "Status code was: " ++ show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
