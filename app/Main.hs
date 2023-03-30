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
runCommand cfg (PageBodyCommand opts) =
    CLI.getPageBody cfg opts.space opts.title
runCommand cfg (PageChildrenCommand opts) =
    CLI.listPageChildren cfg opts.ident
runCommand cfg (PageCreateCommand opts) =
    CLI.createPage
        cfg
        opts.space
        opts.title
        opts.representation
        opts.status
        opts.filePath
runCommand cfg (PageDeleteCommand opts) =
    CLI.deletePage cfg opts.id opts.purge
runCommand cfg (PageGetCommand opts) =
    CLI.getPage cfg opts.ident
runCommand cfg (PageListCommand opts) =
    CLI.listPages
        cfg
        opts.space
        opts.title
        opts.start
        opts.limit
runCommand cfg (PageUpdateCommand opts) =
    CLI.updatePage
        cfg
        opts.id
        opts.newTitle
        opts.newStatus
        opts.newRepresentation
        opts.newBodyFilePath
runCommand cfg (SpacesListCommand opts) =
    CLI.getSpaces cfg opts.start opts.limit opts.spaceType

--------------------------------------------------------------------------------
