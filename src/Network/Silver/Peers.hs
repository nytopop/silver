{- |
Module      :  Network.Silver.Peer
Description :  Peer discovery.
Copyright   :  (c) Eric Izoita 2017
License     :  BSD3

Maintainer  :  ericizoita@gmail.com
Stability   :  experimental
Portability :  portable

This module handles peer discovery. This includes tracker
requests as well as other methods (local, PeX, DHT).
-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Silver.Peers
  ( getTrackerPeers
  ) where

-- Binary Data
import Data.Binary.Get (getWord16be, getWord32be, runGet)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

-- Concurrency
import Control.Concurrent.STM.TVar (readTVar)
import Control.Monad.STM (atomically)

-- Containers
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

-- Networking
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Network.HTTP.Types.URI (renderQuery)
import Network.Socket (SockAddr(..))

-- Internal
import Network.Silver.BEncode (BVal(..), bDecode, key)
import Network.Silver.Client (Client(..))
import Network.Silver.Meta (announce, infoHash)

data TrackerProto
  = TrackerHTTP
  | TrackerUDP

data TrackerResp
  = Fat [BVal]
        Integer
  | Compact ByteString
            Integer
  | Failure ByteString

-- | Get peers and re-request delay from a tracker.
getTrackerPeers :: Client -> IO (Set SockAddr, Integer)
getTrackerPeers c = do
  up <- ar $ clientUploaded c
  down <- ar $ clientDownloaded c
  left <- ar $ clientLeft c
  case announceProto tracker of
    TrackerHTTP -> do
      req <- simpleHTTP $ getRequest $ BS.unpack uri
      resp <- (fmap BS.pack . getResponseBody) req
      case (bDecode resp >>= decodeTracker) of
        Just (Fat peerList interval) -> do
          print "fat"
          return (S.empty, 10)
        Just (Compact peerStr interval) -> do
          print "compact"
          let peers = peers0023 (BL.fromStrict peerStr)
          return (S.fromList peers, interval)
        Just (Failure reason) -> do
          print reason
          return (S.empty, 10)
        Nothing -> do
          return (S.empty, 30)
      where str = BS.pack . show
            params =
              [ ("info_hash", Just $ infoHash meta)
              , ("peer_id", Just $ clientID c)
              , ("port", Just $ str $ clientPort c)
              , ("uploaded", Just $ str up)
              , ("downloaded", Just $ str down)
              , ("left", Just $ str left)
              , ("compact", Just "1")
              ]
            uri = BS.concat [tracker, renderQuery True params]
    TrackerUDP -> return (S.empty, 10)
  where
    ar = atomically . readTVar
    meta = clientMeta c
    tracker = announce meta

-- | Get the tracker protocol associated with an announce uri.
announceProto :: ByteString -> TrackerProto
announceProto uri =
  if BS.isPrefixOf "http://" uri
    then TrackerHTTP
    else if BS.isPrefixOf "udp://" uri
           then TrackerUDP
           else TrackerHTTP -- default to http

-- | Ensure that the neccessary keys are present in a tracker
-- response.
decodeTracker :: BVal -> Maybe TrackerResp
decodeTracker (BDict t) =
  case M.lookup (key "failure") t of
    Just (BStr s) -> Just $ Failure s
    Nothing ->
      case M.lookup (key "interval") t of
        Just (BInt interval) ->
          case M.lookup (key "peers") t of
            Just (BList peers) -> Just $ Fat peers interval
            Just (BStr peers) -> Just $ Compact peers interval
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
decodeTracker _ = Nothing

-- | Parse a BEP 0003 encoded peer list.
{-
peers0003 :: BVal -> [Either ByteString SockAddr]
peers0003 (BList []) = []
peers0003 (BList [(BDict x):xs]) =
  let uri = M.lookup (key "ip") x
      port = M.lookup (key "port") x
  in a
peers0003 _ = []
-}
-- | Parse a BEP 0023 encoded peer list.
peers0023 :: BL.ByteString -> [SockAddr]
peers0023 s
  | s == BL.empty = []
  | mod (BL.length s) 6 /= 0 = []
  | otherwise =
    let pair = do
          addr' <- getWord32be
          port' <- getWord16be
          return (addr', port')
        (cur, next) = BL.splitAt 6 s
        (addr, port) = runGet pair cur
    in SockAddrInet (fromIntegral port) addr : peers0023 next

-- | Get the peers out of a tracker response.
-- The expected input for this function is the "peers" key
-- of a bencoded response.
--
-- Valid encodings: BEP 0003, BEP 0023
peersOf :: BVal -> Maybe (Set (Either ByteString SockAddr))
peersOf (BList xs) = Nothing
peersOf (BStr s)
  | mod (BS.length s) 6 /= 0 = Nothing
  | otherwise =
    let raw = peers0023 $ BL.fromStrict s
        peers = fmap (Right) raw
    in Just $ S.fromList peers
peersOf _ = Nothing
