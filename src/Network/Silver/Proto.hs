{- |
Module      :  Network.Silver.Proto
Description :  Bittorrent peer protocol.
Copyright   :  (c) Eric Izoita 2017
License     :  BSD3

Maintainer  :  ericizoita@gmail.com
Stability   :  experimental
Portability :  portable

This module handles the bittorrent peer protocol.
-}
module Network.Silver.Proto
  (
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Network.Silver.Torrent (Piece, PieceData, Torrent)
import Network.Socket (SockAddr)

-- | An active client.
data Client =
  Client ByteString -- peer id
         SockAddr -- listening socket
         [Torrent] -- active torrents
  deriving (Show, Eq)

-- | An associated peer.
data Peer =
  Peer ByteString -- peer id
       PeerProto -- peer's protocol
       ByteString -- ip addr or dns name
       Int -- port
       PeerStatus -- peer's status
  deriving (Show, Eq)

-- | Peer protocol type.
data PeerProto
  = TCP
  | UTP
  deriving (Show, Eq)

-- | Peer status.
data PeerStatus =
  PeerStatus Bool -- choked
             Bool -- interested
  deriving (Show, Eq)

-- | Protocol message.
data Message
  = MsgChoke
  | MsgUnChoke
  | MsgInterested
  | MsgNotInterested
  | MsgHave Int -- piece index
  | MsgBitfield
  | MsgRequest Int -- piece index
               Int -- begin byte offset
               Int -- length
  | MsgPiece Int -- piece index
             Int -- begin byte offset
             ByteString -- piece data
  | MsgCancel Int -- piece index
              Int -- begin byte offset
              Int -- length
  deriving (Show, Eq)

-- | *** Piece Discovery
-- | Check if a peer has a piece.
hasPiece :: Piece -> Peer -> IO Bool
-- | Get a list of peers with a piece.
withPiece :: Piece -> [Peer] -> IO [Peer]
-- | Get a mapping of pieces to available peers.
getPieceMap :: Map Int Piece -> [Peer] -> IO (Map Int [Peer])
-- | ***
-- | Verify a piece.
isValidPiece :: Piece -> PieceData -> Bool
-- | Try to obtain a piece from a peer.
getPiece :: Piece -> Peer -> IO (Maybe PieceData)
