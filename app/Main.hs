

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}


module Main (
  main
) where


import Aws.Lambda (Context(Context, customContext), addStandaloneLambdaHandler, defaultDispatcherOptions, runLambdaHaskellRuntime)
import Data.IORef (readIORef)
import Language.Marlowe.Runtime.App (handle)
import Language.Marlowe.Runtime.App.Parser (getConfigParser)
import Language.Marlowe.Runtime.App.Types (Config, MarloweRequest(..), MarloweResponse(..))
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag(V1))

import qualified Options.Applicative as O


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
    configParser <- getConfigParser
    config <-
      O.execParser
        $ O.info
          (O.helper {- <*> O.versionOption -} <*> configParser)
          (
            O.fullDesc
              <> O.progDesc "This executable implements an AWS Lambda service for Marlowe Runtime."
              <> O.header "marlowe-lambda: run a marlowe application AWS Lambda service"
          )
    runLambdaHaskellRuntime defaultDispatcherOptions (pure config) id
      $ addStandaloneLambdaHandler "marlowe" handler
