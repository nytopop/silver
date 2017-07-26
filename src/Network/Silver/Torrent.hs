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

-- | TORRENT PROCESS
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
  let blob = mkBlob meta
      info = infoHash meta
      pieces = pieceList meta
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
--    scan blob for verified pieces
--    start a listening socket
--    contact tracker for peers (refresh periodically)
--    begin obtaining pieces from peers
--      get piece data
--      verify
--      write to blob
--
-- This function should block until all pieces are 
-- downloaded.
dl :: Torrent -> IO ()
dl (Torrent meta blob info pieces avail) = do
  print blob
  print info
  print $ (show $ length pieces) ++ " pieces total"
  print avail
  availB blob pieces
  return ()

-- | Download and seed a torrent.
--
-- This function should block forever.
dls :: Torrent -> IO ()
dls t = print 0

-- | Verify a piece.
--
verifyP :: PieceData -> PieceHash -> (PieceHash, Bool)
verifyP piece checksum =
  let sha1 :: ByteString -> Digest SHA1
      sha1 = hash
      newsum = (BS.pack . show . sha1) piece
  in (checksum, checksum == newsum)

-- | Get availability of verified pieces in blob.
--
-- Note : Data within blob that does not hash correctly 
-- will be treated as non-available, and thus will be
-- overwritten throughout a download.
availB :: Blob -> [PieceHash] -> IO (Set PieceHash)
availB blob checksums =
  let len = (fromIntegral $ length checksums) :: Integer
      indices = [0 .. len - 1]
      getPieces = sequence $ map (bGetPiece blob) indices
      verified xs = zipWith verifyP xs checksums
      availOf xs = map fst $ filter (\(_, has) -> has) xs
  in do pieces <- getPieces
        return $ S.fromList $ (availOf . verified) pieces
