

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.Marlowe.Lambda (
  handle
, handler
) where


import Aws.Lambda (Context(Context, customContext))
import Control.Exception (SomeException, catch)
import Data.Aeson
import Data.IORef (readIORef)
import Language.Marlowe.Runtime.Core.Api (renderContractId)
import Language.Marlowe.Lambda.Client (Config, runLambdaWithConfig)
import Language.Marlowe.Lambda.List (listContracts)

import qualified Data.Text as T (pack)
import qualified Data.Vector as V (fromList)


handle :: Value
       -> Config
       -> IO (Either Value Value)
handle _ config =
  let
    format = Array . V.fromList . fmap (String . renderContractId )
  in
    (fmap format . Right <$> runLambdaWithConfig config listContracts)
       `catch` \(err :: SomeException) -> pure $ Left $ String $ T.pack $ show err


handler :: Value
        -> Context Config
        -> IO (Either Value Value)
handler input Context{customContext} =
  do
    config <- readIORef customContext
    handle input config
