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
--   1. decode metainfo
--   2. allocate a blob
--   2.5. scan blob for verified pieces
--        construct a set of available / unavailable pieces
--   3. generate a random peer id
--   4. start a listening socket
--   5. create peer cloud from tracker(s) <|> DHT
--      refresh periodically
--   6. obtain unavailable pieces from peers
--      verify piece
--      write to blob
--      verify from blob
--      add to available set, remove from unavailable 
--   7. continue seeding 
--      ad infinitum <|> until ratio <|> until time
--
--   DESIGN DECISIONS
--   1. every torrent must have a unique sockaddr
--      it suffices to use a different port
--   2. every torrent must have a unique peer id
--
-- Torrent minfo blob pieces (avail, navail) sockaddr
data Torrent =
  Torrent MetaInfo
          Blob
          ByteString -- info hash
          [PieceHash]
          (Set PieceHash, Set PieceHash) -- (avail, not avail)
     --     SockAddr
       --   ByteString -- peer id
  deriving (Show, Eq)

type PieceHash = ByteString

type PieceData = ByteString

-- | Load a torrent from metainfo.
-- this should probably be in IO
--  availB
-- listening socket
-- random peer id
mkTorrent :: MetaInfo -> Torrent
mkTorrent meta =
  let blob = mkBlob meta
      info = infoHash meta
      pieces = pieceList meta
      avail = availB blob pieces
  in Torrent meta blob info pieces avail

-- | Generate a SHA1 info_hash from MetaInfo.
infoHash :: MetaInfo -> ByteString
infoHash (MetaInfo (BDict m)) =
  let sha1 :: ByteString -> Digest SHA1
      sha1 = hash
      s = bEncode (m ! (key "info"))
  in (BS.pack . show . sha1) s

-- | Extract piece list from MetaInfo.
pieceList :: MetaInfo -> [PieceHash]
pieceList _ = []

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
availB :: Blob -> [PieceHash] -> (Set PieceHash, Set PieceHash)
availB blob hashes = (S.empty, S.empty)
