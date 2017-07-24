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
  , PieceHash
  , PieceData
  ) where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.STM ()
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA1)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Network.Silver.BEncode (BVal(..), bEncode, key)
import Network.Silver.Blob (Blob, bGetPiece, bPutPiece, mkBlob)
import Network.Silver.Meta (MetaInfo(..), decodeMeta)
import Network.Silver.Proto (Peer)
import Network.Socket (SockAddr)

-- | TORRENT PROCESS
--   2. dlTorrent
--      scan blob for verified pieces
--      start a listening socket
--      contact tracker for peers (refresh periodically)
--      begin obtaining pieces from peers
--        get piece data
--        verify
--        write to blob
--
--   DESIGN DECISIONS
--   1. every active torrent must have a unique sockaddr
--      it suffices to use a different port
--   2. every active torrent must have a unique peer id
--
data Torrent =
  Torrent MetaInfo
          Blob -- storage
          ByteString -- info hash
          [PieceHash] -- pieces
          (Set PieceHash) -- (available piees)
  deriving (Show, Eq)

type PieceHash = ByteString

type PieceData = ByteString

-- | Load a torrent from metainfo.
mkTorrent :: MetaInfo -> Torrent
mkTorrent meta =
  let blob = mkBlob meta -- done
      info = infoHash meta -- done
      pieces = pieceList meta -- done
      avail = S.empty
  in Torrent meta blob info pieces avail

-- | Generate a SHA1 info_hash from MetaInfo.
infoHash :: MetaInfo -> ByteString
infoHash (MetaInfo (BDict m)) =
  let sha1 :: ByteString -> Digest SHA1
      sha1 = hash
      s = bEncode (m ! (key "info"))
  in (BS.pack . show . sha1) s

split20 :: ByteString -> [ByteString]
split20 xs
  | xs == BS.empty = []
  | otherwise = BS.take 20 xs : split20 (BS.drop 20 xs)

-- | Extract pieces list from MetaInfo.
pieceList :: MetaInfo -> [PieceHash]
pieceList (MetaInfo (BDict m)) =
  let (BDict inf) = m ! (key "info")
      (BStr pieces) = inf ! (key "pieces")
  in split20 pieces

-- | Download a torrent.
--
-- This function will block until all pieces are downloaded.
dl :: Torrent -> IO ()
dl t = print 0

-- | Download and seed a torrent.
--
-- This function will block forever.
dls :: Torrent -> IO ()
dls t = print 0

-- | Verify a piece.
--
-- hash piecedata, equality test
verifyP :: PieceHash -> PieceData -> Bool
verifyP _ _ = True

-- | Get availability of verified pieces in blob.
--
-- Note : Data within blob that does not hash correctly will be
-- treated as empty space in the resulting availability sets.
availB :: Blob -> [PieceHash] -> IO (Set PieceHash)
availB blob hashes = return S.empty
