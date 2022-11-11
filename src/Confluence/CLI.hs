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

getSpaces :: Config -> IO ()
getSpaces cfg = runConfluence cfg API.getSpaces >>= handleCli printSpaces

printSpaces :: SpaceArray -> IO ()
printSpaces arr =
    let spaces = sparrResults arr
    in  printTable $ defaultTable
            [ "Id" : (display . spId <$> spaces)
            , "Name" : (spName <$> spaces)
            , "Key" : (spKey <$> spaces)
            , "Type" : (display . spType <$> spaces)
            ]

--------------------------------------------------------------------------------
