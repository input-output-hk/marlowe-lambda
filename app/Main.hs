

{-# LANGUAGE OverloadedStrings #-}


module Main (
  main
) where


import Aws.Lambda (addStandaloneLambdaHandler, defaultDispatcherOptions, runLambdaHaskellRuntime)
import Data.Default (def)
import Language.Marlowe.Lambda (handler)


main :: IO ()
main =
  runLambdaHaskellRuntime defaultDispatcherOptions def id
    $ addStandaloneLambdaHandler "marlowe" handler
