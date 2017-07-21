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
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Set (Set)
import Network.Silver.BEncode (key)
import Network.Silver.Blob (Blob, bGetPiece, bPutPiece, mkBlob)
import Network.Silver.Meta (MetaInfo)
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
          [PieceHash]
          (Set PieceHash, Set PieceHash) -- (avail, not avail)
          SockAddr
          ByteString -- peer id
  deriving (Show, Eq)

type PieceHash = ByteString

type PieceData = ByteString

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
availB :: Blob -> [PieceHash] -> Maybe (Set PieceHash, Set PieceHash)
availB blob hashes = Nothing
