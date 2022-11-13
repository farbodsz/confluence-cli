--------------------------------------------------------------------------------

module Confluence.CLI
    ( getSpaces
    ) where

import qualified Confluence.API                as API
import           Confluence.Config              ( Config )
import           Confluence.Display
import           Confluence.Error               ( ResponseError
                                                , errorMsg
                                                )
import           Confluence.Monad               ( runConfluence )
import           Confluence.Table
import           Confluence.Types
import qualified Data.Text.IO                  as T

--------------------------------------------------------------------------------

handleCli :: (a -> IO ()) -> Either ResponseError a -> IO ()
handleCli = either (T.putStrLn . errorMsg)

--------------------------------------------------------------------------------

getSpaces :: Int -> Int -> Maybe SpaceType -> Config -> IO ()
getSpaces start limit ty cfg =
    runConfluence cfg (API.getSpaces start limit ty) >>= handleCli printSpaces

printSpaces :: SpaceArray -> IO ()
printSpaces arr =
    let spaces = sparrResults arr
    in  printTable $ defaultTable
            [ "ID" : (display . spId <$> spaces)
            , "NAME" : (spName <$> spaces)
            , "KEY" : (spKey <$> spaces)
            , "TYPE" : (display . spType <$> spaces)
            ]

--------------------------------------------------------------------------------
