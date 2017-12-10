{- |
Module      :  Network.Silver.Meta
Description :  Bittorrent protocol metainfo.
Copyright   :  (c) Eric Izoita 2017
License     :  BSD3

Maintainer  :  ericizoita@gmail.com
Stability   :  experimental
Portability :  portable

This module handles validation and reading of 
metainfo files.
-}
module Network.Silver.Meta
  ( MetaInfo(..)
  -- Content Functions
  , announce
  , PieceList(..)
  , pieceList
  , InfoHash(..)
  , infoHash
  -- Decoding
  , decodeMetaFile
  , decodeMeta
  , isFileDict
  , isInfoDict
  , isMetaInfo
  ) where

-- Crypto
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA1)

-- Binary Data
import Data.ByteString.Base16 (decode)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Containers
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

-- Internal
import Network.Silver.BEncode (BVal(..), bDecode, bEncode, key)

-- | A meta info.
newtype MetaInfo =
  MetaInfo BVal
  deriving (Show, Eq)

-- | A hash of the infodict within a meta info.
newtype InfoHash =
  InfoHash ByteString
  deriving (Show, Eq)

-- | A list of pieces within a meta info.
newtype PieceList =
  PieceList [ByteString]
  deriving (Show, Eq)

-- | Extract announce url from MetaInfo.
announce :: MetaInfo -> ByteString
announce (MetaInfo (BDict m)) =
  let (BStr uri) = m ! (key "announce")
  in uri
announce _ = error "announce of invalid metainfo"

-- | Generate a SHA1 info_hash from MetaInfo.
infoHash :: MetaInfo -> ByteString
infoHash (MetaInfo (BDict m)) =
  let sha1 :: ByteString -> Digest SHA1
      sha1 = hash
      s = bEncode (m ! (key "info"))
      (raw, _) = (decode . BS.pack . show . sha1) s
  in raw
infoHash _ = error "infoHash of invalid metainfo"

-- | Split a byte string into pieces of length 20.
split20 :: ByteString -> [ByteString]
split20 xs
  | xs == BS.empty = []
  | otherwise =
    let cur = BS.take 20 xs
        nxt = BS.drop 20 xs
    in cur : split20 nxt

-- | Extract pieces list from MetaInfo.
pieceList :: MetaInfo -> PieceList
pieceList (MetaInfo (BDict m)) =
  let (BDict inf) = m ! (key "info")
      (BStr pieces) = inf ! (key "pieces")
  in PieceList $ split20 pieces
pieceList _ = error "pieceList of invalid metainfo"

-- | Decode and validate MetaInfo from a file.
decodeMetaFile :: String -> IO (Maybe MetaInfo)
decodeMetaFile f = BS.readFile f >>= \bs -> return $ decodeMeta bs

-- | Decode and validate MetaInfo from a ByteString.
decodeMeta :: ByteString -> Maybe MetaInfo
decodeMeta xs = 
  let check v =
        if isMetaInfo v
          then Just $ MetaInfo v
          else Nothing
  in bDecode xs >>= check

-- | Check whether a BVal is a non-empty BStr.
isBStr :: BVal -> Bool
isBStr (BStr s) = BS.length s > 0
isBStr _ = False

-- | Check whether a BVal is a valid FileDict.
isFileDict :: BVal -> Bool
isFileDict (BDict f) =
  let l =
        case M.lookup (key "length") f of
          Just (BInt _) -> True
          _ -> False
      p =
        case M.lookup (key "path") f of
          Just (BList []) -> False
          Just (BList xs) ->
            let items = map isBStr xs
            in foldr (&&) True items
          _ -> False
  in l && p
isFileDict _ = False

-- | Check whether a BVal is a valid InfoDict.
isInfoDict :: BVal -> Bool
isInfoDict (BDict i) =
  let n =
        case M.lookup (key "name") i of
          Just (BStr _) -> True
          _ -> False
      pl =
        case M.lookup (key "piece length") i of
          Just (BInt _) -> True
          _ -> False
      p =
        case M.lookup (key "pieces") i of
          Just (BStr s) -> (BS.length s) `rem` 20 == 0
          _ -> False
      l =
        case M.lookup (key "length") i of
          Just (BInt _) -> True
          _ -> False
      f =
        case M.lookup (key "files") i of
          Just (BList ld) ->
            let items = map isFileDict ld
            in foldr (&&) True items
          _ -> False
      xor a b = (a || b) && not (a && b)
  in n && pl && p && (xor l f)
isInfoDict _ = False

-- | Check whether a BVal is a valid MetaInfo.
isMetaInfo :: BVal -> Bool
isMetaInfo (BDict m) =
  let announce' =
        case M.lookup (key "announce") m of
          Just (BStr _) -> True
          _ -> False
      info =
        case M.lookup (key "info") m of
          Just i -> isInfoDict i
          _ -> False
  in announce' && info
isMetaInfo _ = False
