

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.Lambda.List (
  allContracts
, followedContracts
, followContract
, unfollowContract
, getContract
) where


import Data.Bifunctor (first)
import Data.Type.Equality ((:~:)(Refl))
import Data.Void (Void, absurd)
import Language.Marlowe.Lambda.Client (runJobClient, runQueryClient, runSyncClient)
import Language.Marlowe.Lambda.Types (Lambda, Services(..))
import Language.Marlowe.Runtime.Core.Api (ContractId, IsMarloweVersion(..), MarloweVersion, assertVersionsEqual)
import Language.Marlowe.Runtime.Discovery.Api (DiscoveryQuery(..), ContractHeader(contractId))
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep, HistoryCommand(..), HistoryQuery(..))
import Network.Protocol.Job.Client (liftCommand)

import qualified Data.Map as M (keys)
import qualified Network.Protocol.Query.Client as Query (ClientStInit(SendMsgRequest), ClientStNext(ClientStNext), ClientStNextCanReject(..), QueryClient(QueryClient), ClientStPage(..))
import qualified Language.Marlowe.Protocol.Sync.Client as Sync (ClientStFollow(ClientStFollow, recvMsgContractNotFound, recvMsgContractFound), ClientStIdle(SendMsgRequestNext, SendMsgDone), ClientStInit(SendMsgFollowContract), ClientStNext(..), ClientStWait(SendMsgCancel), MarloweSyncClient(MarloweSyncClient))


allContracts :: Lambda [ContractId]
allContracts = listContracts GetContractHeaders runDiscoveryQueryClient $ fmap contractId


followedContracts :: Lambda [ContractId]
followedContracts = listContracts GetFollowedContracts runHistoryQueryClient M.keys


listContracts
  :: Monoid a
  => query delimiter Void results
  -> (Services IO -> Query.QueryClient query IO a -> IO a)
  -> (results -> a)
  -> Lambda a
listContracts query run extract =
  let
    handleNextPage previous results nextPage =
      let
        cumulative = previous <> extract results
      in
        pure
          $ maybe
            (Query.SendMsgDone cumulative)
            (flip Query.SendMsgRequestNext . Query.ClientStNext $ handleNextPage cumulative)
            nextPage
  in
    runQueryClient run
      . Query.QueryClient
      . pure
      $ Query.SendMsgRequest query Query.ClientStNextCanReject
        { Query.recvMsgReject = absurd
        , Query.recvMsgNextPage = handleNextPage mempty
        }


followContract :: ContractId -> Lambda (Either String Bool)
followContract = followCommand FollowContract


unfollowContract :: ContractId -> Lambda (Either String Bool)
unfollowContract = followCommand StopFollowingContract


followCommand
  :: Show e
  => (ContractId -> HistoryCommand Void e Bool)
  -> ContractId
  -> Lambda (Either String Bool)
followCommand command contractId' =
  fmap (first show)
    . runJobClient runHistoryJobClient
    . liftCommand
    $ command contractId'


getContract
  :: forall v
  .  IsMarloweVersion v
  => ContractId
  -> Lambda (Either String (CreateStep v, [ContractStep v]))
getContract contractId' =
  let
    next version create previous = Sync.ClientStNext
      { Sync.recvMsgRollBackCreation = pure $ Left "Creation transaction was rolled back."
      , Sync.recvMsgRollBackward = const . pure . Sync.SendMsgDone $ Left "Input application was rolled back."
      , Sync.recvMsgRollForward = \_ steps -> do
          pure . Sync.SendMsgRequestNext $ next version create (previous <> steps)
      , Sync.recvMsgWait = pure . Sync.SendMsgCancel . Sync.SendMsgDone $ Right (create, previous)
      }
  in
    runSyncClient runHistorySyncClient
      . Sync.MarloweSyncClient
      . pure
      $ Sync.SendMsgFollowContract contractId' Sync.ClientStFollow
        { Sync.recvMsgContractNotFound = pure $ Left "Contract not found."
        , Sync.recvMsgContractFound = \_ version create -> do
            case version `assertVersionsEqual` (marloweVersion :: MarloweVersion v) of
              Refl ->  pure . Sync.SendMsgRequestNext $ next version create mempty
        }
