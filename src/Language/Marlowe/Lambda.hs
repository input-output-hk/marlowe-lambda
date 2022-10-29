

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.Marlowe.Lambda (
  handle
, handler
) where


import Aws.Lambda (Context(Context, customContext))
import Control.Exception (SomeException, catch)
import Data.IORef (readIORef)
import Language.Marlowe.Lambda.Client (Config, runLambdaWithConfig)
import Language.Marlowe.Lambda.List (listContracts)


handle :: String
       -> Config
       -> IO (Either String String)
handle _ config =
  do
    (Right . show <$> runLambdaWithConfig config listContracts)
       `catch` \(err :: SomeException) -> pure $ Left $ show err


handler :: String
        -> Context Config
        -> IO (Either String String)
handler input Context{customContext} =
  do
    config <- readIORef customContext
    handle input config
