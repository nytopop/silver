{- |
Module      :  Network.Silver.Blob
Description :  Blob abstraction for torrent contents.
Copyright   :  (c) Eric Izoita 2017
License     :  BSD3

Maintainer  :  ericizoita@gmail.com
Stability   :  experimental
Portability :  portable
-}
module Network.Silver.Blob
  ( Blob
  , mkBlob
  , bPutPiece
  ) where

import Control.Monad (sequence_)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.Silver.BEncode (BVal(..))
import Network.Silver.Meta (MetaInfo(..))
import System.IO

-- | Abstraction to a [set] of files.
data Blob =
  Blob Integer -- piece length
       [File]
  deriving (Show, Eq)

data File =
  File FilePath
       Integer -- length
  deriving (Show, Eq)

-- | Allocate a blob from metainfo data.
-- If this is a single file torrent, (length key)
--   we set the filename and length
--   return $ Blob name length
-- if
-- TODO
mkBlob :: Blob
mkBlob = Blob 256 [File "foo" 512, File "bar" 256, File "baz" 256]

-- | Write a piece at the specified index of the blob.
-- bPutPiece blob piece_index piece_data
bPutPiece :: Blob -> Integer -> ByteString -> IO ()
bPutPiece (Blob pLen fs) idx pData
  | (fromIntegral $ BS.length pData :: Integer) /= pLen =
    error "Piece length does not match!"
  | otherwise =
    let sIdx = idx * pLen
        (a, b, c) = unzip3 $ bGetSplit fs sIdx pData
        res = zipWith3 fPutData a b c
    in sequence_ res

-- bGetSplit fs total_start data
bGetSplit ::
     [File] -> Integer -> ByteString -> [(File, Integer, ByteString)]
bGetSplit [] _ _ = error "Insufficient file space."
bGetSplit (f@(File _ len):fs) idx pData
  | idx < len =
    let endIdx = idx + (fromIntegral $ BS.length pData :: Integer)
        takeN = fromInteger $ len - idx :: Int
        newIdx = idx - (len - idx)
    in case endIdx <= len of
         True -> (f, idx, pData) : []
         False ->
           (f, idx, BS.take takeN pData) :
           bGetSplit fs 0 (BS.drop takeN pData)
  | idx >= len = bGetSplit fs (idx - len) pData

-- | Write data to file starting at the specified byte offset.
-- fPutData file pos data
fPutData :: File -> Integer -> ByteString -> IO ()
fPutData (File path len) fPos fDat =
  let mkHdl = openFile path ReadWriteMode
      fsize = \h -> hSetFileSize h len >> return h
      seek = \h -> hSeek h AbsoluteSeek fPos >> return h
      write = \h -> BS.hPut h fDat >> return h
  in mkHdl >>= fsize >>= seek >>= write >>= \h -> hClose h
-- | Get the piece at the specified index.
-- bGetPiece :: Blob -> Integer -> IO (Maybe ByteString)
