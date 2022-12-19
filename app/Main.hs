

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}


module Main (
  main
) where


import Aws.Lambda (Context(Context, customContext), addStandaloneLambdaHandler, defaultDispatcherOptions, runLambdaHaskellRuntime)
import Data.IORef (readIORef)
import Language.Marlowe.Runtime.Client (handle)
import Language.Marlowe.Runtime.Client.Types (Config, MarloweRequest(..), MarloweResponse(..))
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag(V1))
import System.Environment (getArgs)


handler :: MarloweRequest 'V1
        -> Context Config
        -> IO (Either String (MarloweResponse 'V1))
handler input Context{customContext} =
  do
    config <- readIORef customContext
    handle config input


main :: IO ()
main =
  do
    [configFile] <- getArgs
    config <- read <$> readFile configFile
    runLambdaHaskellRuntime defaultDispatcherOptions (pure config) id
      $ addStandaloneLambdaHandler "marlowe" handler
