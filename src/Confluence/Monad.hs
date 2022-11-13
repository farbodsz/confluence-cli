--------------------------------------------------------------------------------

module Confluence.Monad (
    ConfluenceM,
    runConfluence,
) where

import Confluence.Config (Config)
import Confluence.Error (ResponseError)
import Control.Monad.Except (
    ExceptT,
    runExceptT,
 )
import Control.Monad.Reader (ReaderT (runReaderT))

--------------------------------------------------------------------------------

-- | The Confluence monad represents the context in executing an API command.
type ConfluenceM = ReaderT Config (ExceptT ResponseError IO)

runConfluence :: Config -> ConfluenceM a -> IO (Either ResponseError a)
runConfluence cfg m = runExceptT $ runReaderT m cfg

--------------------------------------------------------------------------------
