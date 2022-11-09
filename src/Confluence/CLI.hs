--------------------------------------------------------------------------------

module Confluence.CLI
    ( getSpaces
    ) where

import qualified Confluence.API                as API
import           Confluence.Config              ( Config )
import           Confluence.Error               ( ResponseError
                                                , errorMsg
                                                )
import           Confluence.Monad               ( runConfluence )
import           Confluence.Types
import           Control.Monad                  ( forM_ )
import qualified Data.Text.IO                  as T

--------------------------------------------------------------------------------

handleCli :: (a -> IO ()) -> Either ResponseError a -> IO ()
handleCli = either (T.putStrLn . errorMsg)

--------------------------------------------------------------------------------

getSpaces :: Config -> IO ()
getSpaces cfg = runConfluence cfg API.getSpaces >>= handleCli printSpaces

printSpaces :: SpaceArray -> IO ()
printSpaces arr = forM_ (sparrResults arr)
    $ \space -> T.putStrLn (spName space <> "  (" <> spKey space <> ")")

--------------------------------------------------------------------------------
