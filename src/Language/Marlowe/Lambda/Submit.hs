

module Language.Marlowe.Lambda.Submit (
  submit
) where


import Data.Bifunctor (second)
import Language.Marlowe.Lambda.Client (runJobClient)
import Language.Marlowe.Lambda.Types (Lambda, Services(..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand(SubmitTx), TxId)
import Network.Protocol.Job.Client (liftCommand)

import qualified Cardano.Api as C (BabbageEra, ScriptDataSupportedInEra(ScriptDataInBabbageEra), Tx, getTxBody, getTxId)


submit
  :: C.Tx C.BabbageEra
  -> Lambda (Either String TxId)
submit tx =
  fmap (second . const . fromCardanoTxId . C.getTxId $ C.getTxBody tx)
    . runJobClient runSyncCommandClient
    . liftCommand
    $ SubmitTx C.ScriptDataInBabbageEra tx
