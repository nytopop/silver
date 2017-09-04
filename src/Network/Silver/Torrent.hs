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
  ( newClient
  , runClient
  , dlTorrent
  ) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
       (TVar(..), newTVar, readTVar, writeTVar)
import Control.Monad.Fix (fix)
import Control.Monad.STM (atomically)
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA1)
import Data.Binary (encode)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Network.HTTP.Types.URI (renderQuery)
import Network.Silver.BEncode (BVal(..), bDecode, bEncode, key)
import Network.Silver.Blob
       (Blob, PieceData(..), bGetPiece, bPutPiece, mkBlob)
import Network.Silver.Meta
       (InfoHash, MetaInfo(..), PieceList(..), announce, decodeMeta,
        infoHash, pieceList)
import Network.Silver.Peers (getTPeers)
import Network.Silver.Proto (Peer)
import Network.Socket (SockAddr)
import System.Random

data Client = Client
  { clientMeta :: MetaInfo
  , clientID :: ByteString
  , clientPort :: Int
  , clientUploaded :: TVar Int
  , clientDownloaded :: TVar Int
  , clientLeft :: TVar Int
  , clientBlob :: Blob
  , clientAvail :: TVar (Map Integer Bool)
  , clientPeers :: TVar (Set Peer)
  }

data Target
  = Download
  | Seed
  deriving (Show)

myport :: Int
myport = 9140

-- Make a new client for a particular meta info dictionary.
-- This function will allocate storage on disk.
newClient :: MetaInfo -> Int -> IO Client
newClient meta port = do
  rng <- getStdGen
  let vals = randoms rng :: [Char]
      id = BS.pack $ take 20 vals
  up <- atomically $ newTVar 0
  down <- atomically $ newTVar 0
  left <- atomically $ newTVar 0
  blob <- mkBlob meta
  avail <- atomically $ newTVar $ M.fromList []
  peers <- atomically $ newTVar $ S.fromList []
  return $
    Client
    { clientMeta = meta
    , clientID = id
    , clientPort = port
    , clientUploaded = up
    , clientDownloaded = down
    , clientLeft = left
    , clientBlob = blob
    , clientAvail = avail
    , clientPeers = peers
    }

-- | Run client.
runClient :: Client -> IO ()
runClient c = do
  tracker <- forkIO $ trackPeers c
  threadDelay (1000 * 1000 * 10)
  return ()

-- | Continually refresh the set of peers in c by periodically
-- making requests to a tracker.
trackPeers :: Client -> IO ()
trackPeers c =
  fix $ \loop -> do
    up <- atomically $ readTVar $ clientUploaded c
    down <- atomically $ readTVar $ clientDownloaded c
    left <- atomically $ readTVar $ clientUploaded c
    let str = BS.pack . show
        tracker = announce meta
        params =
          [ (BS.pack "info_hash", Just $ infoHash meta)
          , (BS.pack "peer_id", Just $ clientID c)
          , (BS.pack "port", Just $ str $ clientPort c)
          , (BS.pack "uploaded", Just $ str up)
          , (BS.pack "downloaded", Just $ str down)
          , (BS.pack "left", Just $ str left)
          , (BS.pack "compact", Just $ str 0) -- MUST SUPPORT COMPACT
          ]
        uri = BS.concat [tracker, renderQuery True params]
    -- resp should be (Maybe BVal)
    bd <- get uri >>= \resp -> return $ bDecode resp
    print bd
    threadDelay (1000 * 1000 * 900)
    loop
  where
    meta = clientMeta c

-- | Download a torrent.
--
-- This function should block until all pieces are 
-- downloaded.
dlTorrent :: MetaInfo -> IO ()
dlTorrent meta =
  let info = infoHash meta
      myid = BS.pack "abcdfdsasdfdkjhgfhjk"
      pieces = pieceList meta
  in do up <- atomically $ newTVar (0 :: Int)
        down <- atomically $ newTVar (0 :: Int)
        left <- atomically $ newTVar (0 :: Int)
        let peers :: Set Peer
            peers = S.fromList []
        availPeers <- atomically $ newTVar peers
        forkIO $
          fix $ \loop -> do
            up' <- atomically $ readTVar up
            down' <- atomically $ readTVar down
            left' <- atomically $ readTVar left
            -- now we construct a uri with these query params
            let tracker = announce meta
                str x = BS.pack $ show x
                params =
                  [ (BS.pack "info_hash", Just info)
                  , (BS.pack "peer_id", Just myid)
                  , (BS.pack "port", Just $ str myport)
                  , (BS.pack "uploaded", Just $ str up')
                  , (BS.pack "downloaded", Just $ str down')
                  , (BS.pack "left", Just $ str left')
                  ]
                uri =
                  BS.concat [tracker, renderQuery True params]
            print uri
            print "trying to get it..."
            resp <- get uri
            curPeers <- atomically $ readTVar availPeers
            print curPeers
            -- oh, we have to construct the request right
            --resp <- get $ announce meta
            --print resp
            -- request a peerlist
            -- parse it
            -- update availPeers
            -- wait for the timeout
            loop
        -- next we setup our blob
        blob <- mkBlob meta
        pieceMap <- scanBlob blob pieces
        availPieces <- atomically $ newTVar pieceMap
        -- spawn a thread to listen for upload reqs
        -- spawn a thread 
        return ()

-- | Get some url
get :: ByteString -> IO ByteString
get uri = do
  req <- simpleHTTP (getRequest (BS.unpack uri))
  resp <- getResponseBody req
  return $ BS.pack resp

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
