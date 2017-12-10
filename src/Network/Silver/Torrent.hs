{- |
Module      :  Network.Silver.Torrent
Description :  Torrent downloads / management.
Copyright   :  (c) Eric Izoita 2017
License     :  BSD3

Maintainer  :  ericizoita@gmail.com
Stability   :  experimental
Portability :  portable
-}
module Network.Silver.Torrent
  ( runClient
  ) where

-- Binary Data
import Data.Binary (decodeOrFail, encode)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

-- Concurrency & Flow Control
import Control.Concurrent
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Monad.Fix (fix)
import Control.Monad.STM (atomically)

-- Crypto
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA1)

-- Containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- Networking
import Network.Socket
       (Family(AF_INET), SockAddr(..), SocketType(Stream), connect,
        socket)
import Network.Socket.ByteString.Lazy (recv, send)

-- Internal
import Network.Silver.Blob (Blob, PieceData(..), bGetPiece)
import Network.Silver.Client (Client(..))
import Network.Silver.Meta (PieceList(..), infoHash, pieceList)
import Network.Silver.Peers (getTrackerPeers)
import Network.Silver.Proto (Handshake(..))

{-
-- Which peer source to use.
data PeerSource
  = Tracker
  | DHT

-- Execution target.
data Target
  = Download
  | Seed
  | Parse
  | Verify
  deriving (Show, Eq)

-- Downloading strategy.
data Strategy
  = Stream -- goes sequentially
  | Optimal -- max speed
  | Greedy -- max speed, greedy
-}
-- SO.
-- we need to keep track of a few things:
-- L0: current _network_ status of each _SockAddr_
--     this should be handled by the lower level network api
-- L1: current _protocol_ status of each _Peer_
--     store in: Map ByteString PeerStatus
--
-- Map ByteString Socket     : sockets by peer id
-- Map ByteString (PeerStatus, PeerStatus) : status by peer id
--
-- | Run client.
runClient :: Client -> IO ()
runClient c = do
  _ <- forkIO $ trackPeers c
  pieces <- scanBlob (clientBlob c) $ pieceList meta
  atomically $ writeTVar (clientAvail c) pieces
  -- 1. start a thread that listens for connections
  -- 2. establish connections to all known peers
  -- peers <- atomically $ readTVar (clientPeers c)
  threadDelay (1000 * 1000 * 100)
  return ()
  return ()
  where
    meta = clientMeta c

-- | Continually refresh the set of peers in c by periodically
-- making requests to a tracker.
trackPeers :: Client -> IO ()
trackPeers c =
  fix $ \loop -> do
    (peers, interval) <- getTrackerPeers c
    print "Receiving peers..."
    print peers
    _ <- mapM ((flip talkToPeer) c) $ S.toList peers
    atomically $ do
      current <- readTVar (clientPeers c)
      -- forkIO
      -- connect
      -- insert to 'Map ByteString Socket'
      -- receive message
      -- decode message
      -- send to 'Chan Message'
      let updated = S.union current peers
      --    newpeers = S.difference current updated
      writeTVar (clientPeers c) updated
    putStrLn ("Requesting more peers in " ++ (show interval) ++ " seconds")
    threadDelay (1000 * 1000 * (fromInteger interval))
    loop

talkToPeer :: SockAddr -> Client -> IO (ThreadId)
talkToPeer addr c =
  forkIO $ do
    let hs = Handshake (infoHash $ clientMeta c) (clientID c)
        hsmsg = encode hs
    sock <- socket AF_INET Stream 0
    connect sock addr
    -- welp, that makes sense
    -- we have to use uTP to
    -- hole punch the target network...
    print "we've connected..."
    -- send our handshake
    sent <- send sock hsmsg
    print ("sent " ++ show sent ++ " bytes")
    resp <- recv sock 16384
    case mDec resp of
      Nothing -> return ()
      Just h -> do
        print h

mDec :: BL.ByteString -> Maybe Handshake
mDec d =
  case decodeOrFail d of
    Left _ -> Nothing
    Right (_, _, h) -> Just h

-- | Verify a piece.
--
verifyP :: PieceData -> ByteString -> Bool
verifyP (PieceData piece) checksum =
  let sha1 :: ByteString -> Digest SHA1
      sha1 = hash
      newsum = (BS.pack . show . sha1) piece
  in checksum == newsum

-- | Scan for pieces in blob. Performs checksum verification
-- on all stored pieces, so it may take some time.
--
-- Note: Data within the blob that does not hash correctly 
-- will be treated as non-available, and thus will be
-- overwritten throughout a download.
scanBlob :: Blob -> PieceList -> IO (Map Integer Bool)
scanBlob blob (PieceList checksums) =
  let len = (fromIntegral $ length checksums) :: Integer
      indices = [0 .. len - 1]
      getPieces = sequence $ map (bGetPiece blob) indices
      have xs = zipWith verifyP xs checksums
  in do print blob
        pieces <- getPieces
        return $ M.fromList $ zip indices (have pieces)
