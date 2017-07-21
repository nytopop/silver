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
  ( Peer
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Network.Socket (SockAddr, Socket)

data PeerCloud =
  PeerCloud ByteString -- peer id
            ByteString -- info hash
            [Peer] -- known peer list .. (set might be better)
  deriving (Show, Eq)

data Peer =
  Peer ByteString -- peer id
       SockAddr -- addr
  deriving (Show, Eq)

data PeerStatus
  = Dead
  | Alive Bool -- choked
          Bool -- interested
  deriving (Show, Eq)

data PeerConn =
  PeerConn Socket -- active socket
           SockAddr -- addr
  deriving (Show, Eq)

--peerInsert :: PeerCloud -> Peer -> PeerCloud
--peerDelete :: PeerCloud -> Peer -> PeerCloud
--cGetPiece :: PeerCloud -> Int -> IO ByteString
-- | Return value of a message.
data MessageStatus
  = MsgFail
  | MsgSuccess
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
