

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}


module Language.Marlowe.Lambda.Types (
  Config(..)
, Services(..)
, RunClient
, Lambda(..)
, MarloweRequest(..)
, MarloweResponse(..)
) where


import Control.Applicative (Alternative)
import Control.Monad.Base (MonadBase)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Default (Default(..))
import Data.String (fromString)
import Language.Marlowe (POSIXTime(..))
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.ChainSync.Api (Address, Lovelace(..), ScriptHash(..), TokenName, TxId)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Discovery.Api (DiscoveryQuery)
import Language.Marlowe.Runtime.History.Api
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Job.Client (JobClient)
import Network.Protocol.Query.Client (QueryClient)
import Network.Socket (HostName, PortNumber)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as M


data Config =
  Config
  { historyHost :: HostName
  , historyCommandPort :: PortNumber
  , historyQueryPort :: PortNumber
  , historySyncPort :: PortNumber
  , discoveryHost :: HostName
  , discoveryQueryPort :: PortNumber
  , txHost :: HostName
  , txCommandPort :: PortNumber
  }
    deriving (Read, Show)

instance Default Config where
  def =
    Config
    { historyHost = "127.0.0.1"
    , historyCommandPort = 3717
    , historyQueryPort = 3718
    , historySyncPort = 3719
    , discoveryHost = "127.0.0.1"
    , discoveryQueryPort = 3721
    , txHost = "127.0.0.1"
    , txCommandPort = 3723
    }


data Services m =
  Services
  { runHistoryJobClient :: RunClient m (JobClient HistoryCommand)
  , runHistoryQueryClient :: RunClient m (QueryClient HistoryQuery)
  , runHistorySyncClient :: RunClient m MarloweSyncClient
  , runDiscoveryQueryClient :: RunClient m (QueryClient DiscoveryQuery)
  , runTxJobClient :: RunClient m (JobClient MarloweTxCommand)
  }


-- | A monad type for Marlowe Lambda programs.
newtype Lambda a = Lambda { runLambda :: ReaderT (Services IO) IO a }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadBase IO, MonadBaseControl IO, MonadFail, MonadFix, MonadIO)


-- | A function signature for running a client for some protocol in some monad m.
type RunClient m client = forall a. client m a -> m a


data MarloweRequest v =
    List
  | Followed
  | Follow
    { reqContractId :: ContractId
    }
  | Unfollow
    { reqContractId :: ContractId
    }
  | Get
    { reqContractId :: ContractId
    }
  | Create
    { reqContract :: Contract v
    , reqRoles :: M.Map TokenName Address
    , reqMinUtxo :: Lovelace
    , reqAddresses :: [Address]
    , reqChange :: Address
    }
  | Apply
    { reqContractId :: ContractId
    , reqInputs :: Redeemer v
    , reqValidityLowerBound :: POSIXTime
    , reqValidityUpperBound :: POSIXTime
    , reqAddresses :: [Address]
    , reqChange :: Address
    }
  | Withdraw
    { reqContractId :: ContractId
    , reqRole :: TokenName
    , reqAddresses :: [Address]
    , reqChange :: Address
    }

instance A.FromJSON (MarloweRequest 'V1) where
  parseJSON =
    A.withObject "MarloweRequest"
      $ \o ->
        (o A..: "request" :: A.Parser String)
          >>= \case
            "list" -> pure List
            "followed" -> pure Followed
            "follow" -> do
                          reqContractId <- fromString <$> o A..: "contractId"
                          pure Follow{..}
            "unfollow" -> do
                            reqContractId <- fromString <$> o A..: "contractId"
                            pure Unfollow{..}
            "get" -> do
                       reqContractId <- fromString <$> o A..: "contractId"
                       pure Get{..}
            "create" -> do
                          reqContract <- o A..: "contract"
                          reqRoles <- M.mapKeys fromString . M.map fromString <$> (o A..: "roles" :: A.Parser (M.Map String String))
                          reqMinUtxo <- Lovelace <$> o A..: "minUtxO"
                          reqAddresses <- fmap fromString <$> o A..: "addresses"
                          reqChange <- fromString <$> o A..: "change"
                          pure Create{..}
            "apply" -> do
                         reqContractId <- fromString <$> o A..: "contractId"
                         reqInputs <- o A..: "inputs"
                         reqValidityLowerBound <- POSIXTime <$> o A..: "validityLowerBound"
                         reqValidityUpperBound <- POSIXTime <$> o A..: "validityUpperBound"
                         reqAddresses <- fmap fromString <$> o A..: "addresses"
                         reqChange <- fromString <$> o A..: "change"
                         pure Apply{..}
            "withdraw" -> do
                            reqContractId <- fromString <$> o A..: "contractId"
                            reqRole <- fromString <$> o A..: "role"
                            reqAddresses <- fmap fromString <$> o A..: "addresses"
                            reqChange <- fromString <$> o A..: "change"
                            pure Withdraw{..}
            request -> fail $ "Invalid request: " <> request <> "."

instance A.ToJSON (MarloweRequest 'V1) where
  toJSON List = A.object ["request" A..= ("list" :: String)]
  toJSON Followed = A.object ["request" A..= ("followed" :: String)]
  toJSON Follow{..} =
    A.object
      [ "request" A..=  ("follow" :: String)
      , "contractId" A..= renderContractId reqContractId
      ]
  toJSON Unfollow{..} =
    A.object
      [ "request" A..=  ("unfollow" :: String)
      , "contractId" A..= renderContractId reqContractId
      ]
  toJSON Get{..} =
    A.object
      [ "request" A..= ("get" :: String)
      , "contractId" A..= renderContractId reqContractId
      ]
  toJSON Create{..} =
    A.object
      [ "request" A..= ("create" :: String)
      , "contract" A..= reqContract
      , "minUtxo" A..= unLovelace reqMinUtxo
      , "roles" A..= M.mapKeys show reqRoles
      , "addresses" A..= reqAddresses
      , "change" A..= reqChange
      ] 
  toJSON Apply{..} =
    A.object
      [ "request" A..= ("apply" :: String)
      , "inputs" A..= reqInputs
      , "validityLowerBound" A..= getPOSIXTime reqValidityLowerBound
      , "validityUpperBound" A..= getPOSIXTime reqValidityUpperBound
      , "addresses" A..= reqAddresses
      , "change" A..= reqChange
      ] 
  toJSON Withdraw{..} =
    A.object
      [ "request" A..= ("withdraw" :: String)
      , "role" A..= reqRole
      , "addresses" A..= reqAddresses
      , "change" A..= reqChange
      ]


data MarloweResponse v =
    Contracts
    { resContractIds :: [ContractId]
    }
  | FollowResult
    { resResult :: Bool
    }
  | Info
    { resCreation :: CreateStep v
    , resSteps :: [ContractStep v]
    }
  | Body
    { resTransactionId :: TxId
    , resTransactionBody :: A.Value
    }

instance A.FromJSON (MarloweResponse 'V1) where
  parseJSON =
    A.withObject "MarloweResponse"
      $ \o ->
        (o A..: "response" :: A.Parser String)
          >>= \case
            "contracts" -> do
                             resContractIds <- fmap fromString <$> o A..: "contractIds"
                             pure Contracts{..}
            "result" -> do
                          resResult <- o A..: "result"
                          pure FollowResult{..}
            "info" -> do
                        resCreation <- contractCreationFromJSON =<< o A..: "creation"
                        resSteps <- mapM contractStepFromJSON =<< o A..: "steps"
                        pure Info{..}
            "body" -> do
                        resTransactionId <- fromString <$> o A..: "contractId"
                        resTransactionBody <- o A..: "body"
                        pure Body{..}
            response -> fail $ "Invalid response: " <> response <> "."

instance A.ToJSON (MarloweResponse 'V1) where
  toJSON Contracts{..} =
    A.object
      [ "response" A..= ("contracts" :: String)
      , "contractIds" A..= fmap renderContractId resContractIds
      ]
  toJSON FollowResult{..} =
    A.object
      [ "response" A..= ("result" :: String)
      , "result" A..= resResult
      ]
  toJSON Info{..} =
    A.object
      [ "response" A..= ("info" :: String)
      , "creation" A..= contractCreationToJSON resCreation
      , "steps" A..= fmap contractStepToJSON resSteps
      ]
  toJSON Body{..} =
    A.object
      [ "response" A..= ("body" :: String)
      , "contractId" A..= resTransactionId
      , "body" A..= resTransactionBody
      ]


contractCreationFromJSON :: A.Value ->  A.Parser (CreateStep 'V1)
contractCreationFromJSON _ = error "Not implemented"  -- FIXME


contractCreationToJSON :: CreateStep 'V1-> A.Value
contractCreationToJSON CreateStep{..} =
  A.object
    [ "output" A..= transactionScriptOutputToJSON createOutput
    , "payoutValidatorHash" A..= BS8.unpack (unScriptHash payoutValidatorHash)
    ]


contractStepFromJSON :: A.Value ->  A.Parser (ContractStep 'V1)
contractStepFromJSON _ = error "Not implemented"  -- FIXME


contractStepToJSON :: ContractStep 'V1-> A.Value
contractStepToJSON (ApplyTransaction Transaction{..}) =
  A.object
    [ "step" A..= ("apply" :: String)
    , "txId" A..= transactionId
    , "contractId" A..= renderContractId contractId
    , "redeemer" A..= redeemer
    , "scriptOutput" A..= fmap transactionScriptOutputToJSON (scriptOutput output)
    , "payouts" A..= M.map payoutToJSON (payouts output)
    ]
contractStepToJSON (RedeemPayout RedeemStep{..}) =
  A.object
    [ "step" A..= ("payout" :: String)
    , "utxo" A..= utxo
    , "redeemingTx" A..= redeemingTx
    , "datumm" A..= datum
    ]


transactionScriptOutputToJSON :: TransactionScriptOutput 'V1 -> A.Value
transactionScriptOutputToJSON TransactionScriptOutput{..} =
  A.object
    [ "address" A..= address
    , "assets" A..= assets
    , "utxo" A..= utxo
    , "datum" A..= datum
    ]


payoutToJSON :: Payout 'V1 -> A.Value
payoutToJSON Payout{..} =
  A.object
    [ "address" A..= address
    , "assets" A..= assets
    , "datum" A..= datum
    ]
