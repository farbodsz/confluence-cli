--------------------------------------------------------------------------------

module Confluence.API
    ( testApi
    ) where

import           Confluence.API.Request
import           Confluence.Config              ( Config(..) )
import           Confluence.Types
import           Control.Monad                  ( forM_ )
import qualified Data.Text.IO                  as T

--------------------------------------------------------------------------------
-- API endpoint functions

getSpaces :: Config -> IO ()
getSpaces cfg = do
    e_spaces <- handleApi cfg "space" []
    case e_spaces of
        Left  e           -> print e
        Right space_array -> forM_ (sparrResults space_array)
            $ \space -> T.putStrLn (spName space)

--------------------------------------------------------------------------------
-- Entry point

testApi :: Config -> IO ()
testApi cfg = getSpaces cfg >>= print

--------------------------------------------------------------------------------
