

module Language.Marlowe.Lambda.List (
  listContracts
) where


import Data.Void (absurd)
import Language.Marlowe.Lambda.Client (Lambda, Services(..), runQueryClient)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.History.Api (HistoryQuery(..))
import Network.Protocol.Query.Client (ClientStInit(SendMsgRequest), ClientStNext(ClientStNext), ClientStNextCanReject(ClientStNextCanReject, recvMsgReject, recvMsgNextPage), ClientStPage(SendMsgRequestNext, SendMsgDone), QueryClient(QueryClient))

import qualified Data.Map as M (keys)


listContracts :: Lambda [ContractId]
listContracts =
  let
    handleNextPage previous results nextPage =
      let
        cumulative = previous <> M.keys results
      in
        pure
          $ maybe
            (SendMsgDone cumulative)
            (flip SendMsgRequestNext $ ClientStNext $ handleNextPage cumulative)
            nextPage
  in
    runQueryClient runHistoryQueryClient
      $ QueryClient
      $ pure
      $ SendMsgRequest GetFollowedContracts ClientStNextCanReject
        { recvMsgReject = absurd
        , recvMsgNextPage = handleNextPage mempty
        }
