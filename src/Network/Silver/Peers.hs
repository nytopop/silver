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
module Network.Silver.Peers
  (
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Network.Silver.BEncode (BVal(..), key)
import Network.Silver.Meta (MetaInfo(..))
import Network.Silver.Proto (Peer)
import Network.Socket (SockAddr)

-- | A verified tracker response.
newtype TrackerResponse =
  TrackerResponse BVal
  deriving (Show, Eq)

-- | get peers from trackers in metainfo
-- 1. get the announce url
-- 2. do the tracker request
--    will need downloader info
--    take as parameters
-- 3. decode the bval
-- 4. return ? something
getTPeers :: MetaInfo -> IO [Peer]
getTPeers (MetaInfo (BDict mi)) =
  let a = 1
  in return []
