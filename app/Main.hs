--------------------------------------------------------------------------------

module Main (main) where

import Confluence.CLI qualified as CLI
import Confluence.Commands
import Confluence.Config
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative (execParser)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    cmd <- execParser cliArgs
    e_config <- loadConfig
    case e_config of
        Left err -> T.putStrLn $ configErrMsg err
        Right cfg -> runCommand cfg cmd

configErrMsg :: ConfigLoadError -> T.Text
configErrMsg (NoConfigFoundErr path) =
    "Config file does not exist: " <> T.pack path
configErrMsg (InvalidConfigErr contents) = "Invalid config file:\n" <> contents

runCommand :: Config -> ConfluenceCmd -> IO ()
runCommand cfg (ContentCreateCommand opts) =
    CLI.createContent
        cfg
        opts.space
        opts.title
        opts.representation
        opts.status
        opts.contentType
        opts.filePath
runCommand cfg (ContentDeleteCommand opts) =
    CLI.deleteContent cfg opts.id opts.purge
runCommand cfg (ContentInfoCommand opts) =
    CLI.getContentInfo cfg opts.ident
runCommand cfg (ContentListCommand opts) =
    CLI.listContent
        cfg
        opts.space
        opts.title
        opts.start
        opts.limit
runCommand cfg (ContentUpdateCommand opts) =
    CLI.updateContent
        cfg
        opts.id
        opts.newTitle
        opts.newType
        opts.newStatus
        opts.newRepresentation
        opts.newBodyFilePath
runCommand cfg (SpacesListCommand opts) =
    CLI.listSpaces cfg opts.start opts.limit opts.spaceType

--------------------------------------------------------------------------------
