

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
import Data.Bifunctor (second)
import Data.IORef (readIORef)
import Language.Marlowe.Lambda.Build (buildApplication, buildCreation, buildWithdrawal)
import Language.Marlowe.Lambda.Client (runLambdaWithConfig)
import Language.Marlowe.Lambda.List (allContracts, followContract, followedContracts, getContract, unfollowContract)
import Language.Marlowe.Lambda.Types (Config, MarloweRequest(..), MarloweResponse(..), mkBody)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(MarloweV1), MarloweVersionTag(V1))


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
        Create{..} -> second mkBody <$> buildCreation MarloweV1 reqContract reqRoles reqMinUtxo reqAddresses reqChange reqCollateral
        Apply{..} -> second mkBody <$> buildApplication MarloweV1 reqContractId reqInputs reqValidityLowerBound reqValidityUpperBound reqAddresses reqChange reqCollateral
        Withdraw{..} -> second mkBody <$> buildWithdrawal MarloweV1 reqContractId reqRole reqAddresses reqChange reqCollateral
      
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
