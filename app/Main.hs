

{-# LANGUAGE OverloadedStrings #-}


module Main (
  main
) where


import Aws.Lambda

import qualified Language.Marlowe.Lambda as Marlowe


main :: IO ()
main =
  runLambdaHaskellRuntime defaultDispatcherOptions (pure ()) id
    $ addStandaloneLambdaHandler "handler" Marlowe.handler
