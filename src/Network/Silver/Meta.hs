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

TODO : Either -> Maybe
-}
module Network.Silver.Meta
  ( MetaInfo(..)
  -- Content Functions
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

import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA1)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))
import Network.Silver.BEncode (BVal(..), bDecode, bEncode, key)
import System.IO (FilePath)

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

-- | Generate a SHA1 info_hash from MetaInfo.
infoHash :: MetaInfo -> InfoHash
infoHash (MetaInfo (BDict m)) =
  let sha1 :: ByteString -> Digest SHA1
      sha1 = hash
      s = bEncode (m ! (key "info"))
  in InfoHash $ (BS.pack . show . sha1) s

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

-- | Decode and validate MetaInfo from a file.
decodeMetaFile :: FilePath -> IO (Either String MetaInfo)
decodeMetaFile f = do
  xs <- BS.readFile f
  return (decodeMeta xs)

-- | Decode and validate MetaInfo from a ByteString.
decodeMeta :: ByteString -> Either String MetaInfo
decodeMeta xs =
  case bDecode xs of
    Left msg -> Left msg
    Right val ->
      case isMetaInfo val of
        False -> Left "Invalid MetaInfo!"
        True -> Right $ MetaInfo val

-- | Check whether a BVal is a non-empty BStr.
isBStr :: BVal -> Bool
isBStr (BStr s) = BS.length s > 0
isBStr _ = False

-- | Check whether a BVal is a valid FileDict.
isFileDict :: BVal -> Bool
isFileDict (BDict f) =
  let l =
        case M.lookup (key "length") f of
          Nothing -> False
          Just (BInt _) -> True
      p =
        case M.lookup (key "path") f of
          Nothing -> False
          Just (BList []) -> False
          Just (BList xs) ->
            let items = map isBStr xs
            in foldr (&&) True items
  in l && p
isFileDict _ = False

-- | Check whether a BVal is a valid InfoDict.
isInfoDict :: BVal -> Bool
isInfoDict (BDict i) =
  let n =
        case M.lookup (key "name") i of
          Nothing -> False
          Just (BStr _) -> True
      pl =
        case M.lookup (key "piece length") i of
          Nothing -> False
          Just (BInt _) -> True
      p =
        case M.lookup (key "pieces") i of
          Nothing -> False
          Just (BStr s) -> (BS.length s) `rem` 20 == 0
      l =
        case M.lookup (key "length") i of
          Nothing -> False
          Just (BInt _) -> True
      f =
        case M.lookup (key "files") i of
          Nothing -> False
          Just (BList ld) ->
            let items = map isFileDict ld
            in foldr (&&) True items
      xor a b = (a || b) && not (a && b)
  in n && pl && p && (xor l f)
isInfoDict _ = False

-- | Check whether a BVal is a valid MetaInfo.
isMetaInfo :: BVal -> Bool
isMetaInfo (BDict m) =
  let announce =
        case M.lookup (key "announce") m of
          Nothing -> False
          Just (BStr _) -> True
      info =
        case M.lookup (key "info") m of
          Nothing -> False
          Just i -> isInfoDict i
  in announce && info
isMetaInfo _ = False
