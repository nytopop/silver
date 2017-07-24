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
import Network.Silver.Meta (MetaInfo(..))
import Network.Socket (SockAddr)
import Network.Silver.Proto (Peer)
