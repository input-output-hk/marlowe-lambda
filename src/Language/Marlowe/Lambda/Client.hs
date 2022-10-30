

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}


module Language.Marlowe.Lambda.Client (
  Config(..)
, Services(..)
, Lambda(..)
, runQueryClient
, runLambdaWithConfig
) where


import Control.Applicative (Alternative)
import Control.Exception (Exception, bracket, bracketOnError, throwIO)
import Control.Monad.Base (MonadBase)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Data.ByteString.Lazy (ByteString)
import Data.Default (Default(..))
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient, marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Language.Marlowe.Runtime.Discovery.Api (DiscoveryQuery)
import Language.Marlowe.Runtime.History.Api (HistoryCommand, HistoryQuery)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Channel (socketAsChannel)
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.Job.Client (JobClient, jobClientPeer)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Query.Client (QueryClient, hoistQueryClient, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Socket (AddrInfo, HostName, PortNumber, SocketType(..), addrSocketType, defaultHints, getAddrInfo, addrAddress, addrSocketType, close, connect, openSocket)
import Network.TypedProtocol (Driver(startDState), Peer, PeerRole(..), runPeerWithDriver)
import Network.TypedProtocol.Codec (Codec)


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
  def = localConfig


localConfig :: Config
localConfig =
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


-- | A function signature for running a client for some protocol in some monad m.
type RunClient m client = forall a. client m a -> m a


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
