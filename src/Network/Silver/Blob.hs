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
  , bGetPiece
  , bPutPiece
  ) where

import Control.Monad (sequence, sequence_)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))
import Network.Silver.BEncode (BVal(..), key)
import Network.Silver.Meta (MetaInfo(..))
import qualified System.FilePath.Posix as SF
import System.IO
       (FilePath, IOMode(ReadMode, ReadWriteMode), SeekMode(AbsoluteSeek),
        hClose, hSeek, hSetFileSize, openFile)

-- | Abstracts a piece indexed, file delineated binary storage mechanism.
-- TODO : Dynamic space allocation
--        during the file write process
--        mkBlob becomes a pure function
data Blob =
  Blob Integer -- piece length
       [File]
  deriving (Show, Eq)

data File =
  File FilePath
       Integer -- file length
  deriving (Show, Eq)

-- | Make a blob.
mkBlob :: MetaInfo -> Blob
mkBlob (MetaInfo (BDict mi)) =
  let (BDict inf) = mi ! (key "info")
      (BStr name) = inf ! (key "name")
      (BInt pLen) = inf ! (key "piece length")
      len = M.lookup (key "length") inf
      files = M.lookup (key "files") inf
      fxs =
        case (len, files) of
          (Just (BInt x), Nothing) -> File (BS.unpack name) x : []
          (Nothing, Just (BList fs)) -> mkMulti name fs
  in Blob pLen fxs

-- | Make a file list.
mkMulti :: ByteString -> [BVal] -> [File]
mkMulti name [] = []
mkMulti name ((BDict x):xs) =
  let (BInt len) = x ! (key "length")
      (BList paths) = x ! (key "path")
      path = SF.joinPath $ mkPaths paths
  in File path len : mkMulti name xs

mkPaths :: [BVal] -> [FilePath]
mkPaths [] = []
mkPaths ((BStr f):fs) = BS.unpack f : mkPaths fs
mkPaths _ = []

-- | Given a list of files, a starting byte offset, and number of bytes 
-- to distribute, fSplit returns a list of files that make up the data 
-- distribution. Each returned 3 tuple represents an implicated file, 
-- starting byte offset relative to the file, and length in bytes of data.
--
-- fSplit files byte_offset byte_length
--  -> [ (file, byte_offset, byte_length) ... ]
--
-- A runtime exception will be triggered if:
--   files does not have at least byte_length byte capacity
--   byte_offset < 0
--   byte_length < 0
fSplit :: [File] -> Integer -> Integer -> [(File, Integer, Integer)]
fSplit _ _ 0 = []
fSplit [] _ _ = error "Not enough allocated space!"
fSplit (f@(File _ len):fs) idx dLen
  | idx < 0 = error "Negative index!"
  | dLen < 0 = error "Negative data length!"
  | idx < len =
    let endIdx = idx + dLen
        takeN = len - idx
    in case endIdx <= len of
         True -> (f, idx, dLen) : []
         False -> (f, idx, takeN) : fSplit fs 0 (dLen - takeN)
  | idx >= len = fSplit fs (idx - len) dLen

-- | Split a byte string into a list of byte strings of the given sizes.
--
-- If the length of the source byte string is not equal to the sum of 
-- lengths provided, a runtime exception will be triggered.
bSplit :: ByteString -> [Integer] -> [ByteString]
bSplit _ [] = []
bSplit bd bs@(r:bl)
  | dLen /= sum bs = error "Mismatched data / offsets!"
  | otherwise =
    let bytes :: Int
        bytes = fromIntegral r
    in BS.take bytes bd : bSplit (BS.drop bytes bd) bl
  where
    dLen :: Integer
    dLen = fromIntegral $ BS.length bd

-- | Write a piece to a blob.
-- 
-- If the length of the piece's data is not equal to the blob's
-- piece length, a runtime exception will be triggered.
bPutPiece :: Blob -> Integer -> ByteString -> IO ()
bPutPiece (Blob pLen fs) pIdx pData
  | dLen /= pLen = error "Mismatched piece / data length!"
  | otherwise =
    let startIdx = pIdx * dLen
        (fss, bo, bl) = unzip3 $ fSplit fs startIdx dLen
        bs = bSplit pData bl
        actions = zipWith3 fPutData fss bo bs
    in sequence_ actions
  where
    dLen :: Integer
    dLen = fromIntegral $ BS.length pData

-- | Write data to a file starting at the specified byte offset. 
--
-- If the data is longer than the file can accept, a runtime 
-- exception will be triggered.
fPutData :: File -> Integer -> ByteString -> IO ()
fPutData (File path len) fPos fData
  | fPos + dLen > len = error "Too much data for file!"
  | otherwise =
    let mkHdl = openFile path ReadWriteMode
        fsize h = hSetFileSize h len >> return h
        seek h = hSeek h AbsoluteSeek fPos >> return h
        write h = BS.hPut h fData >> return h
    in mkHdl >>= fsize >>= seek >>= write >>= \h -> hClose h
  where
    dLen :: Integer
    dLen = fromIntegral $ BS.length fData

-- | Get the piece at the given piece index.
bGetPiece :: Blob -> Integer -> IO ByteString
bGetPiece (Blob pLen fs) pIdx =
  let startIdx = pIdx * pLen
      (fss, bo, bl) = unzip3 $ fSplit fs startIdx pLen
      actions = zipWith3 fGetData fss bo bl
      chunk xs = return $ BS.concat xs
  in sequence actions >>= chunk

-- | Get data from a file, starting at the specified byte offset.
--
-- If you try reading too much data from the file, a runtime
-- exception will be triggered.
fGetData :: File -> Integer -> Integer -> IO ByteString
fGetData (File path len) fPos dLen
  | fPos + dLen > len = error "Not enough data in file"
  | otherwise =
    let bytes :: Int
        bytes = fromInteger dLen
        mkHdl = openFile path ReadMode
        seek h = hSeek h AbsoluteSeek fPos >> return h
        get h = BS.hGet h bytes >>= \bs -> return (h, bs)
        fin (h, bs) = hClose h >> return bs
    in mkHdl >>= seek >>= get >>= fin
