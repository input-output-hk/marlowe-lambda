

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.Marlowe.Lambda (
  handle
, handler
) where


import Aws.Lambda (Context(Context, customContext))
import Control.Exception (SomeException, catch)
import Data.IORef (readIORef)
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag(V1))
import Language.Marlowe.Lambda.Client (runLambdaWithConfig)
import Language.Marlowe.Lambda.List (allContracts, followContract, followedContracts, getContract, unfollowContract)
import Language.Marlowe.Lambda.Types (Config, MarloweRequest(..), MarloweResponse(..))


handle :: MarloweRequest 'V1
       -> Config
       -> IO (Either String (MarloweResponse 'V1))
handle request config =
  let
    run =
      case request of
        List -> Right . Contracts <$> allContracts
        Followed -> Right . Contracts <$> followedContracts
        Follow{..} -> fmap FollowResult <$> followContract reqContractId
        Unfollow{..} -> fmap FollowResult <$> unfollowContract reqContractId
        Get{..} -> fmap (uncurry Info) <$> getContract reqContractId
        _ -> error "Not implemented."
      
  in
    runLambdaWithConfig config run
       `catch` \(err :: SomeException) -> pure . Left $ show err


handler :: MarloweRequest 'V1
        -> Context Config
        -> IO (Either String (MarloweResponse 'V1))
handler input Context{customContext} =
  do
    config <- readIORef customContext
    handle input config
