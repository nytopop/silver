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
{-# LANGUAGE OverloadedStrings #-}

module Network.Silver.Proto
  ( PeerStatus(..)
  , Handshake(..)
  , Message(..)
  ) where

-- Binary Data
import Data.Binary (Binary(..))
import Data.Binary.Get (Get, getByteString)
import Data.Binary.Put (putByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Data.Word (Word32, Word8)

-- Things to add
-- - network hole punching over uTP through
--   an intermediary unrestricted STUN host
data PeerStatus
  = Dead
  | Alive Bool -- choked
          Bool -- interested
  deriving (Show)

data Handshake =
  Handshake ByteString -- info hash
            ByteString -- peer id
  deriving (Show)

instance Binary Handshake where
  get = do
    len <- get :: Get Word8
    case len of
      19 -> do
        proto <- getByteString (fromIntegral len)
        if proto == "Bittorrent protocol"
          then do
            _ <- get :: Get Word32
            _ <- get :: Get Word32
            info <- getByteString 20
            peer <- getByteString 20
            return $ Handshake info peer
          else fail "invalid protocol string"
      _ -> fail "invalid handshake length"
  put (Handshake info peer)
    | BS.length info /= 20 = fail "invalid info hash length"
    | BS.length peer /= 20 = fail "invalid peer id length"
    | otherwise = do
      put (19 :: Word8)
      putByteString "Bittorrent protocol"
      put (0 :: Word32)
      put (0 :: Word32)
      putByteString info
      putByteString peer

data Message
  = MsgKeepAlive
  | MsgChoke
  | MsgUnChoke
  | MsgInterested
  | MsgNotInterested
  | MsgHave Word32 -- piece index
  | MsgBitfield ByteString
  | MsgRequest Word32 -- piece index
               Word32 -- begin byte offset
               Word32 -- length
  | MsgPiece Word32 -- piece index
             Word32 -- begin byte offset
             ByteString -- piece data
  | MsgCancel Word32 -- piece index
              Word32 -- begin byte offset
              Word32 -- length
  deriving (Show)

instance Binary Message where
  get = do
    len <- get :: Get Word32
    case len of
      0 -> return MsgKeepAlive
      _ -> do
        msgId <- get :: Get Word8
        case msgId of
          0 -> return MsgChoke
          1 -> return MsgUnChoke
          2 -> return MsgInterested
          3 -> return MsgNotInterested
          4 -> MsgHave <$> (get :: Get Word32)
          5 -> MsgBitfield <$> getByteString (fromIntegral len - 1)
          6 -> MsgRequest <$> get <*> get <*> get
          7 -> do
            let block = getByteString (fromIntegral len - 9)
            MsgPiece <$> get <*> get <*> block
          8 -> MsgCancel <$> get <*> get <*> get
          _ -> fail "invalid protocol message id"
  put MsgKeepAlive = do
    put (0 :: Word32)
  put MsgChoke = do
    put (1 :: Word32) >> put (0 :: Word8)
  put MsgUnChoke = do
    put (1 :: Word32) >> put (1 :: Word8)
  put MsgInterested = do
    put (1 :: Word32) >> put (2 :: Word8)
  put MsgNotInterested = do
    put (1 :: Word32) >> put (3 :: Word8)
  put (MsgHave idx) = do
    put (5 :: Word32) >> put (4 :: Word8) >> put idx
  put (MsgBitfield field) = do
    let len = fromIntegral $ 1 + BS.length field
    put (len :: Word32) >> put (5 :: Word8)
    putByteString field
  put (MsgRequest index offset len) = do
    put (13 :: Word32) >> put (6 :: Word8)
    put index >> put offset >> put len
  put (MsgPiece index offset block) = do
    let len = fromIntegral $ 9 + BS.length block
    put (len :: Word32) >> put (7 :: Word8)
    put index >> put offset >> put block
  put (MsgCancel index offset len) = do
    put (13 :: Word32) >> put (8 :: Word8)
    put index >> put offset >> put len
