{- |
Module      :  Network.Silver.Torrent
Description :  Clients.
Copyright   :  (c) Eric Izoita 2017
License     :  BSD3

Maintainer  :  ericizoita@gmail.com
Stability   :  experimental
Portability :  portable

-}
module Network.Silver.Torrent
  ( Torrent
  , Piece
  , PieceData
  , Chunk
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.Silver.BEncode (key)
import Network.Silver.Meta (MetaInfo)
import Network.Silver.Proto (Peer)

-- | A single active torrent.
data Torrent =
  Torrent MetaInfo -- Metainfo for torrent
          ByteString -- info hash
          Integer -- downloaded bytes
          Integer -- uploaded bytes
          Integer -- remaining bytes
          (Map Int Piece) -- piece map
          (Map Int [Peer]) -- network map
  deriving (Show, Eq)

type Piece = ByteString

type PieceData = ByteString

-- | Construct a piece map from MetaInfo.
mkPieceMap :: MetaInfo -> Map Int Piece
-- | Construct an info hash from MetaInfo.
mkInfoHash :: MetaInfo -> ByteString
-- | Check the availability of a network map.
availability :: Map Int [Peer] -> Float
