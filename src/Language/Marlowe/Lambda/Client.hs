

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}


module Language.Marlowe.Lambda.Client (
  runQueryClient
, runLambdaWithConfig
) where


import Control.Exception (Exception, bracket, bracketOnError, throwIO)
import Control.Monad.Trans.Control (liftBaseWith)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Data.ByteString.Lazy (ByteString)
import Language.Marlowe.Lambda.Types
import Language.Marlowe.Protocol.Sync.Client (marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Network.Channel (socketAsChannel)
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.Job.Client (jobClientPeer)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Query.Client (QueryClient, hoistQueryClient, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Socket (AddrInfo, SocketType(..), addrSocketType, defaultHints, getAddrInfo, addrAddress, addrSocketType, close, connect, openSocket)
import Network.TypedProtocol (Driver(startDState), Peer, PeerRole(..), runPeerWithDriver)
import Network.TypedProtocol.Codec (Codec)


-- | Run a History Query client.
runQueryClient
  :: (Services IO -> QueryClient q IO a -> IO a)
  -> QueryClient q Lambda a
  -> Lambda a
runQueryClient query client =
  do
    services <- Lambda ask
    liftBaseWith
      $ \runInBase ->
        query services
          $ hoistQueryClient runInBase client


runLambdaWithConfig
  :: Config
  -> Lambda a
  -> IO a
runLambdaWithConfig Config{..} lambda = do
  historyJobAddr <-  resolve historyHost historyCommandPort
  historyQueryAddr <- resolve historyHost historyQueryPort
  historySyncAddr <- resolve historyHost historySyncPort
  discoveryQueryAddr <- resolve discoveryHost discoveryQueryPort
  txJobAddr <- resolve txHost txCommandPort
  runReaderT (runLambda lambda) Services
    { runHistoryJobClient = runClientPeerOverSocket  historyJobAddr Network.Protocol.Job.Codec.codecJob jobClientPeer
    , runHistoryQueryClient = runClientPeerOverSocket  historyQueryAddr codecQuery queryClientPeer
    , runHistorySyncClient = runClientPeerOverSocket  historySyncAddr codecMarloweSync marloweSyncClientPeer
    , runTxJobClient = runClientPeerOverSocket  txJobAddr Network.Protocol.Job.Codec.codecJob jobClientPeer
    , runDiscoveryQueryClient = runClientPeerOverSocket  discoveryQueryAddr codecQuery queryClientPeer
    }
  where
    resolve host port =
      head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just $ show port)


-- | Run a client as a typed protocols peer over a socket.
runClientPeerOverSocket
  :: Exception ex
  => AddrInfo -- ^ Socket address to connect to
  -> Codec protocol ex IO ByteString -- ^ A codec for the protocol
  -> (forall a. client IO a -> Peer protocol 'AsClient st IO a) -- ^ Interpret the client as a protocol peer
  -> RunClient IO client
runClientPeerOverSocket addr codec clientToPeer client = bracket open close $ \socket -> do
  let channel = socketAsChannel socket
  let driver = mkDriver throwIO codec channel
  let peer = clientToPeer client
  fst <$> runPeerWithDriver driver peer (startDState driver)
  where
    open = bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      pure sock
