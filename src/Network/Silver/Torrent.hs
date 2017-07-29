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
  ( PieceHash
  , PieceData
  , dlTorrent
  ) where

import Control.Concurrent.STM.TVar
       (TVar(..), newTVar, readTVar, writeTVar)
import Control.Monad.STM (atomically)
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA1)
import qualified Data.ByteString.Base16 as Hex
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

-- TODO make these in newtype
type PieceHash = ByteString

type PieceData = ByteString

-- | Generate a SHA1 info_hash from MetaInfo.
infoHash :: MetaInfo -> ByteString
infoHash (MetaInfo (BDict m)) =
  let sha1 :: ByteString -> Digest SHA1
      sha1 = hash
      s = bEncode (m ! (key "info"))
  in (BS.pack . show . sha1) s

-- | Split a byte string into pieces of length 20.
split20 :: ByteString -> [ByteString]
split20 xs
  | xs == BS.empty = []
  | otherwise =
    let cur = Hex.encode $ BS.take 20 xs
        nxt = BS.drop 20 xs
    in cur : split20 nxt

-- | Extract pieces list from MetaInfo.
pieceList :: MetaInfo -> [PieceHash]
pieceList (MetaInfo (BDict m)) =
  let (BDict inf) = m ! (key "info")
      (BStr pieces) = inf ! (key "pieces")
  in split20 pieces

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
verifyP :: PieceData -> PieceHash -> Bool
verifyP piece checksum =
  let sha1 :: ByteString -> Digest SHA1
      sha1 = hash
      newsum = (BS.pack . show . sha1) piece
  in checksum == newsum

-- | Scan for pieces in blob.
--
-- Note : Data within blob that does not hash correctly 
-- will be treated as non-available, and thus will be
-- overwritten throughout a download.
scanBlob :: Blob -> [PieceHash] -> IO (Map Integer Bool)
scanBlob blob checksums =
  let len = (fromIntegral $ length checksums) :: Integer
      indices = [0 .. len - 1]
      getPieces = sequence $ map (bGetPiece blob) indices
      have xs = zipWith verifyP xs checksums
  in do pieces <- getPieces
        return $ M.fromList $ zip indices (have pieces)
