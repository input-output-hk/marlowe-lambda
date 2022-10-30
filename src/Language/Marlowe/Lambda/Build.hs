

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.Lambda.Build (
  buildCreation
, buildApplication
, buildWithdrawal
) where


import Data.Bifunctor (bimap, second)
import Data.Time ( UTCTime, secondsToNominalDiffTime ) 
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Data.Void (Void)
import Language.Marlowe (POSIXTime(..))
import Language.Marlowe.Lambda.Client (runJobClient)
import Language.Marlowe.Lambda.Types (Lambda, Services(..))
import Language.Marlowe.Runtime.ChainSync.Api (Address, Lovelace(..), TokenName, TxOutRef)
import Language.Marlowe.Runtime.Core.Api (ContractId, IsMarloweVersion(..), MarloweVersion)
import Language.Marlowe.Runtime.Transaction.Api (ApplyInputsError, CreateError, MarloweTxCommand(Withdraw, Create, ApplyInputs), WalletAddresses(WalletAddresses), WithdrawError, mkMint)
import Network.Protocol.Job.Client (liftCommand)

import qualified Cardano.Api as C (BabbageEra, TxBody)
import qualified Data.List.NonEmpty as NE (fromList)
import qualified Data.Map.Strict as M (Map, null, toList)
import qualified Data.Set as S (fromList)


buildCreation
  :: Show (CreateError v)
  => MarloweVersion v
  -> Contract v
  -> M.Map TokenName Address
  -> Lovelace
  -> [Address]
  -> Address
  -> [TxOutRef]
  -> Lambda (Either String (C.TxBody C.BabbageEra))
buildCreation version contract roles minUtxo =
  let
    roles' =
      if M.null roles
        then Nothing
        else Just . Right . mkMint . fmap (second (, Left 1)) . NE.fromList . M.toList $ roles
  in 
    build show snd $ \w -> Create Nothing version w roles' mempty minUtxo contract


buildApplication
  :: Show (ApplyInputsError v)
  => MarloweVersion v
  -> ContractId
  -> Redeemer v
  -> POSIXTime
  -> POSIXTime
  -> [Address]
  -> Address
  -> [TxOutRef]
  -> Lambda (Either String (C.TxBody C.BabbageEra))
buildApplication version contractId' redeemer lower upper =
  build show id $ \w -> ApplyInputs version w contractId' (utcTime lower) (utcTime upper) redeemer


buildWithdrawal
  :: Show (WithdrawError v)
  => MarloweVersion v
  -> ContractId
  -> TokenName
  -> [Address]
  -> Address
  -> [TxOutRef]
  -> Lambda (Either String (C.TxBody C.BabbageEra))
buildWithdrawal version contractId' role =
  build show id $ \w -> Withdraw version w contractId' role


build
  :: (err -> String)
  -> (result -> C.TxBody C.BabbageEra)
  -> (WalletAddresses -> MarloweTxCommand Void err result)
  -> [Address]
  -> Address
  -> [TxOutRef]
  -> Lambda (Either String (C.TxBody C.BabbageEra))
build showError getBody command addresses change collaterals =
  do
    let
      command' = command $ WalletAddresses change (S.fromList addresses) (S.fromList collaterals)
    fmap (bimap showError getBody) . runJobClient runTxJobClient . liftCommand $ command'


utcTime :: POSIXTime -> Maybe UTCTime
utcTime = Just . posixSecondsToUTCTime . secondsToNominalDiffTime . (/ 1000) . fromInteger . getPOSIXTime
