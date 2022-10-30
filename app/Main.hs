

{-# LANGUAGE OverloadedStrings #-}


module Main (
  main
) where


import Aws.Lambda (addStandaloneLambdaHandler, defaultDispatcherOptions, runLambdaHaskellRuntime)
import Language.Marlowe.Lambda (handler)
import System.Environment (getArgs)


main :: IO ()
main =
  do
    [configFile] <- getArgs
    config <- read <$> readFile configFile
    runLambdaHaskellRuntime defaultDispatcherOptions (pure config) id
      $ addStandaloneLambdaHandler "marlowe" handler
