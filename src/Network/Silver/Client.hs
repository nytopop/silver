{- |
Module      :  Network.Silver.Client
Description :  Clients.
Copyright   :  (c) Eric Izoita 2017
License     :  BSD3

Maintainer  :  ericizoita@gmail.com
Stability   :  experimental
Portability :  portable

This module handles the networking side and generating local clients.
-}
module Network.Silver.Client
  (
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.Silver.Meta
import Network.Socket (SockAddr)

-- | Represents an active client.
data Client =
  Client SockAddr -- listening socket
         ByteString -- peer id
         [Torrent] -- active torrents
  deriving (Show, Eq)

-- | Represents a single active torrent.
data Torrent =
  Torrent MetaInfo -- Metainfo for torrent
          ByteString -- info hash for metainfo
          Integer -- downloaded bytes
          Integer -- uploaded bytes
  deriving (Show, Eq)

data TStatus
  = Started
  | Completed
  | Stopped
  deriving (Show, Eq)
