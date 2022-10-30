

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
import Language.Marlowe.Runtime.ChainSync.Api (Address, Lovelace(..), TokenName, TxId)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Discovery.Api (DiscoveryQuery)
import Language.Marlowe.Runtime.History.Api
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Job.Client (JobClient)
import Network.Protocol.Query.Client (QueryClient)
import Network.Socket (HostName, PortNumber)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
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
    { reqContractIds :: [ContractId]
    }
  | Unfollow
    { reqContractIds :: [ContractId]
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
                          reqContractIds <- fmap fromString <$> o A..: "ids"
                          pure Follow{..}
            "unfollow" -> do
                            reqContractIds <- fmap fromString <$> o A..: "ids"
                            pure Unfollow{..}
            "get" -> do
                       reqContractId <- fromString <$> o A..: "id"
                       pure Get{..}
            "create" -> do
                          reqContract <- o A..: "contract"
                          reqRoles <- M.mapKeys fromString . M.map fromString <$> (o A..: "roles" :: A.Parser (M.Map String String))
                          reqMinUtxo <- Lovelace <$> o A..: "minUtxO"
                          reqAddresses <- fmap fromString <$> o A..: "addresses"
                          reqChange <- fromString <$> o A..: "change"
                          pure Create{..}
            "apply" -> do
                         reqContractId <- fromString <$> o A..: "id"
                         reqInputs <- o A..: "inputs"
                         reqValidityLowerBound <- POSIXTime <$> o A..: "validityLowerBound"
                         reqValidityUpperBound <- POSIXTime <$> o A..: "validityUpperBound"
                         reqAddresses <- fmap fromString <$> o A..: "addresses"
                         reqChange <- fromString <$> o A..: "change"
                         pure Apply{..}
            "withdraw" -> do
                            reqContractId <- fromString <$> o A..: "id"
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
      , "ids" A..= fmap renderContractId reqContractIds
      ]
  toJSON Unfollow{..} =
    A.object
      [ "request" A..=  ("unfollow" :: String)
      , "ids" A..= fmap renderContractId reqContractIds
      ]
  toJSON Get{..} =
    A.object
      [ "request" A..= ("get" :: String)
      , "id" A..= renderContractId reqContractId
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
  | Info
    { resContractId :: ContractId
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
                             resContractIds <- fmap fromString <$> o A..: "ids"
                             pure Contracts{..}
            "info" -> do
                        resContractId <- fromString <$> o A..: "id"
                        resSteps <- mapM contractStepFromJSON =<< o A..: "steps"
                        pure Info{..}
            "body" -> do
                        resTransactionId <- fromString <$> o A..: "id"
                        resTransactionBody <- o A..: "body"
                        pure Body{..}
            response -> fail $ "Invalid response: " <> response <> "."


instance A.ToJSON (MarloweResponse 'V1) where
  toJSON Contracts{..} =
    A.object
      [ "response" A..= ("contracts" :: String)
      , "ids" A..= fmap renderContractId resContractIds
      ]
  toJSON Info{..} =
    A.object
      [ "response" A..= ("info" :: String)
      , "id" A..= renderContractId resContractId
      , "steps" A..= fmap contractStepToJSON resSteps
      ]
  toJSON Body{..} =
    A.object
      [ "response" A..= ("body" :: String)
      , "id" A..= resTransactionId
      , "body" A..= resTransactionBody
      ]


contractStepFromJSON :: A.Value ->  A.Parser (ContractStep 'V1)
contractStepFromJSON _ = error "Not implemented"  -- FIXME


contractStepToJSON :: ContractStep 'V1-> A.Value
contractStepToJSON (ApplyTransaction Transaction{}) =
  A.object
    [ "step" A..= ("apply" :: String)
    ]
contractStepToJSON (RedeemPayout RedeemStep{}) =
  A.object
    [ "step" A..= ("payout" :: String)
    ]
