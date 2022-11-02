--------------------------------------------------------------------------------

module Main where

import Confluence.API (testApi)
import Confluence.CLI
import Options.Applicative (execParser)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    cmd <- execParser cliArgs
    runCli cmd

runCli :: CliCommand -> IO ()
runCli CmdApi = putStrLn "API command called" >> testApi

--------------------------------------------------------------------------------
