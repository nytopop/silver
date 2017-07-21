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
  ( Torrent
  , Piece
  , PieceData
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Network.Silver.BEncode (key)
import Network.Silver.Meta (MetaInfo)
import Network.Silver.Proto (Peer)
import System.IO

-- | TORRENT PROCESS
--   1. load a metainfo file
--   2. create or open file handles for torrent
--   3. start a listener for incoming connections
--      the listener should have access to the Blob + MetaInfo + PeerID
--   4. obtain peer list from tracker or dht
--   5. create a PeerCloud from the peer list
--   6. use PeerCloud to obtain pieces from peers
--   7. verify each piece, then write to Blob
--   8. when blob is complete, you're done and can remove
--         peer cloud
--
data Torrent =
  Torrent MetaInfo -- Metainfo for torrent
          Integer -- downloaded bytes
          Integer -- uploaded bytes
          Integer -- remaining bytes
          (Map Int Piece) -- piece map
  deriving (Show, Eq)

type Piece = ByteString

type PieceData = ByteString
-- | Construct a piece map from MetaInfo.
--mkPieceMap :: MetaInfo -> Map Int Piece
-- | Construct an info hash from MetaInfo.
--mkInfoHash :: MetaInfo -> ByteString
-- | Verify a piece.
--isValidPiece :: Piece -> PieceData -> Bool
-- | Check the availability of a network map.
--availability :: Map Int [Peer] -> Float
