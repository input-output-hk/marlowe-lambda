

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
, mkBody
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
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (Address, Lovelace(..), TokenName, TxId, TxOutRef, fromBech32, toBech32)
import Language.Marlowe.Runtime.Core.Api (ContractId, IsMarloweVersion(Redeemer, Contract), MarloweVersionTag(V1), Transaction(Transaction, output, redeemer, validityUpperBound, validityLowerBound, blockHeader, contractId, transactionId), TransactionScriptOutput(..), Payout(..), TransactionOutput(scriptOutput, payouts), renderContractId)
import Language.Marlowe.Runtime.Discovery.Api (DiscoveryQuery)
import Language.Marlowe.Runtime.History.Api ( CreateStep(..), ContractStep(..), HistoryCommand, HistoryQuery, RedeemStep(RedeemStep, datum, redeemingTx, utxo))
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Job.Client (JobClient)
import Network.Protocol.Query.Client (QueryClient)
import Network.Socket (HostName, PortNumber)

import qualified Data.Aeson.Types as A (FromJSON(parseJSON), Parser, ToJSON(toJSON), Value(String), (.:), (.=), object, parseFail, withObject) -- (FromJSON(parseJSON), Parser, ToJSON(toJSON), Value, (.=), (.:), object, withObject)
import qualified Data.Map.Strict as M (Map, map, mapKeys)
import qualified Data.Text as T (Text)
import qualified Cardano.Api as C (BabbageEra, TxBody, getTxId, serialiseToTextEnvelope)


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
  , verbose :: Bool
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
    , verbose = False
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
    -- TODO: Add staking.
    -- TODO: Add metadata.
    , reqAddresses :: [Address]
    , reqChange :: Address
    , reqCollateral :: [TxOutRef]
    }
  | Apply
    { reqContractId :: ContractId
    , reqInputs :: Redeemer v
    , reqValidityLowerBound :: POSIXTime
    , reqValidityUpperBound :: POSIXTime
    -- TODO: Add metadata.
    , reqAddresses :: [Address]
    , reqChange :: Address
    , reqCollateral :: [TxOutRef]
    }
  | Withdraw
    { reqContractId :: ContractId
    , reqRole :: TokenName
    -- TODO: Add metadata.
    , reqAddresses :: [Address]
    , reqChange :: Address
    , reqCollateral :: [TxOutRef]
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
                          reqMinUtxo <- Lovelace <$> o A..: "minUtxo"
                          reqAddresses <- mapM addressFromJSON =<< o A..: "addresses"
                          reqChange <- addressFromJSON =<< o A..: "change"
                          reqCollateral <- fmap fromString <$> o A..: "collateral"
                          pure Create{..}
            "apply" -> do
                         reqContractId <- fromString <$> o A..: "contractId"
                         reqInputs <- o A..: "inputs"
                         reqValidityLowerBound <- POSIXTime <$> o A..: "validityLowerBound"
                         reqValidityUpperBound <- POSIXTime <$> o A..: "validityUpperBound"
                         reqAddresses <- mapM addressFromJSON =<< o A..: "addresses"
                         reqChange <- addressFromJSON =<< o A..: "change"
                         reqCollateral <- fmap fromString <$> o A..: "collateral"
                         pure Apply{..}
            "withdraw" -> do
                            reqContractId <- fromString <$> o A..: "contractId"
                            reqRole <- fromString <$> o A..: "role"
                            reqAddresses <- mapM addressFromJSON =<< o A..: "addresses"
                            reqChange <- addressFromJSON =<< o A..: "change"
                            reqCollateral <- fmap fromString <$> o A..: "collateral"
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
      , "addresses" A..= fmap addressToJSON reqAddresses
      , "change" A..= addressToJSON reqChange
      , "collateral" A..= reqCollateral
      ] 
  toJSON Apply{..} =
    A.object
      [ "request" A..= ("apply" :: String)
      , "inputs" A..= reqInputs
      , "validityLowerBound" A..= getPOSIXTime reqValidityLowerBound
      , "validityUpperBound" A..= getPOSIXTime reqValidityUpperBound
      , "addresses" A..= fmap addressToJSON reqAddresses
      , "change" A..= addressToJSON reqChange
      , "collateral" A..= reqCollateral
      ] 
  toJSON Withdraw{..} =
    A.object
      [ "request" A..= ("withdraw" :: String)
      , "role" A..= reqRole
      , "addresses" A..= fmap addressToJSON reqAddresses
      , "change" A..= addressToJSON reqChange
      , "collateral" A..= reqCollateral
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
    { resContractId :: ContractId
    , resTransactionId :: TxId
    , resTransactionBody :: C.TxBody C.BabbageEra
    }

{-
instance A.FromJSON (MarloweResponse 'V1) where
  parseJSON =
    A.withObject "MarloweResponse"
      $ \o ->
        (o A..: "response" :: A.Parser String)
          >>= \case
            "contracts" -> Contracts . fmap fromString <$> o A..: "contractIds"
            "result" -> FollowResult <$> o A..: "result"
            "info" -> do
                        resCreation <- contractCreationFromJSON =<< o A..: "creation"
                        resSteps <- mapM contractStepFromJSON =<< o A..: "steps"
                        pure Info{..}
            "body" -> fmap mkBody <$> txBodyFromJSON =<< o A..: "body"
            response -> fail $ "Invalid response: " <> response <> "."
-}

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
      , "contractId" A..= renderContractId resContractId
      , "txId" A..= C.getTxId resTransactionBody
      , "body" A..= txBodyToJSON resTransactionBody
      ]


{-
contractCreationFromJSON :: A.Value ->  A.Parser (CreateStep 'V1)
contractCreationFromJSON _ = error "Not implemented"
-}


contractCreationToJSON :: CreateStep 'V1-> A.Value
contractCreationToJSON CreateStep{..} =
  A.object
    [ "output" A..= transactionScriptOutputToJSON createOutput
    , "payoutValidatorHash" A..= filter (/= '"') (show payoutValidatorHash)
    ]


{-
contractStepFromJSON :: A.Value ->  A.Parser (ContractStep 'V1)
contractStepFromJSON _ = error "Not implemented"
-}


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


{-
txBodyFromJSON :: C.TextEnvelope -> A.Parser (C.TxBody C.BabbageEra)
txBodyFromJSON envelope =
  do
    case C.deserialiseFromTextEnvelope (C.AsTxBody C.AsBabbageEra) envelope of
      Left msg -> fail $ show msg
      Right body -> pure body
-}


txBodyToJSON :: C.TxBody C.BabbageEra -> A.Value
txBodyToJSON body =
  let
    envelope = C.serialiseToTextEnvelope Nothing body
  in
    A.toJSON envelope


mkBody :: ContractId -> C.TxBody C.BabbageEra -> MarloweResponse v
mkBody resContractId resTransactionBody =
  let
    resTransactionId = fromCardanoTxId $ C.getTxId resTransactionBody
  in
    Body{..}


addressFromJSON :: T.Text -> A.Parser Address
addressFromJSON = maybe (A.parseFail "Failed decoding Bech32 address.") pure . fromBech32


addressToJSON :: Address -> A.Value
addressToJSON = maybe (error "Failed encoding Bech32 address.") A.String . toBech32
