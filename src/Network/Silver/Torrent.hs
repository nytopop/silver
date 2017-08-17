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
  ( dlTorrent
  ) where

-- Concurrency
import Control.Concurrent.STM.TVar
       (TVar(..), newTVar, readTVar, writeTVar)
import Control.Monad.STM (atomically)

-- Data
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA1)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

-- Internal
import Network.Silver.BEncode (BVal(..), bEncode, key)
import Network.Silver.Blob
       (Blob, PieceData(..), bGetPiece, bPutPiece, mkBlob)
import Network.Silver.Meta
       (InfoHash, MetaInfo(..), PieceList(..), decodeMeta, infoHash,
        pieceList)
import Network.Silver.Proto (Peer)
import Network.Socket (SockAddr)

-- | Download a torrent.
--
-- This function should block until all pieces are 
-- downloaded.
dlTorrent :: MetaInfo -> IO ()
dlTorrent meta =
  let info = infoHash meta
      pieces = pieceList meta
  in do blob <- mkBlob meta
        scn <- scanBlob blob pieces
        avail <- atomically $ newTVar scn
        return ()

-- | Verify a piece.
--
verifyP :: PieceData -> ByteString -> Bool
verifyP (PieceData piece) checksum =
  let sha1 :: ByteString -> Digest SHA1
      sha1 = hash
      newsum = (BS.pack . show . sha1) piece
  in checksum == newsum

-- | Scan for pieces in blob.
--
-- Note : Data within blob that does not hash correctly 
-- will be treated as non-available, and thus will be
-- overwritten throughout a download.
scanBlob :: Blob -> PieceList -> IO (Map Integer Bool)
scanBlob blob (PieceList checksums) =
  let len = (fromIntegral $ length checksums) :: Integer
      indices = [0 .. len - 1]
      getPieces = sequence $ map (bGetPiece blob) indices
      have xs = zipWith verifyP xs checksums
  in do pieces <- getPieces
        return $ M.fromList $ zip indices (have pieces)
