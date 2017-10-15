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
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

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

-- Internal
import Network.Silver.Blob (Blob, PieceData(..), bGetPiece)
import Network.Silver.Client (Client(..))
import Network.Silver.Meta (PieceList(..))
import Network.Silver.Peers (getTrackerPeers)

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

-- | Run client.
runClient :: Client -> IO ()
runClient c = do
  tracker <- forkIO $ trackPeers c
  mmm <-
    forkIO $
    fix $ \loop -> do
      peers <- atomically $ readTVar $ clientPeers c
      print peers
      threadDelay (1000 * 1000 * 2)
      loop
  -- scanBlob blob pieces
  threadDelay (1000 * 1000 * 100)
  return ()

-- | Continually refresh the set of peers in c by periodically
-- making requests to a tracker.
trackPeers :: Client -> IO ()
trackPeers c =
  fix $ \loop -> do
    (peers, interval) <- getTrackerPeers c
    atomically $ do
      current <- readTVar (clientPeers c)
      let updated = S.union current peers
      writeTVar (clientPeers c) updated
    print peers
    print interval
    threadDelay (1000 * 1000 * (fromInteger interval))
    loop -- can be conditional for exit


-- | Verify a piece.
--
verifyP :: PieceData -> ByteString -> Bool
verifyP (PieceData piece) checksum =
  let sha1 :: ByteString -> Digest SHA1
      sha1 = hash
      newsum = (BS.pack . show . sha1) piece
  in checksum == newsum

-- | Scan for pieces in blob.
--
-- Note : Data within blob that does not hash correctly 
-- will be treated as non-available, and thus will be
-- overwritten throughout a download.
scanBlob :: Blob -> PieceList -> IO (Map Integer Bool)
scanBlob blob (PieceList checksums) =
  let len = (fromIntegral $ length checksums) :: Integer
      indices = [0 .. len - 1]
      getPieces = sequence $ map (bGetPiece blob) indices
      have xs = zipWith verifyP xs checksums
  in do pieces <- getPieces
        return $ M.fromList $ zip indices (have pieces)
