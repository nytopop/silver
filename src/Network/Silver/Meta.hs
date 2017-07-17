{- |
Module      :  Network.Silver.Meta
Description :  Bittorrent protocol metainfo.
Copyright   :  (c) Eric Izoita 2017
License     :  BSD3

Maintainer  :  ericizoita@gmail.com
Stability   :  experimental
Portability :  portable

This module handles validation and reading of metainfo files.
-}
module Network.Silver.Meta
  ( MetaInfo
  , decodeMetaFile
  , decodeMeta
  , infoHash
  ) where

import Crypto.Hash
import Crypto.Hash.Algorithms (SHA1)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))
import Network.Silver.BEncode (BVal(..), bDecode, bEncode)
import System.IO (FilePath)

-- | A verified meta info.
newtype MetaInfo =
  MetaInfo BVal
  deriving (Show, Eq)

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
      case checkMeta val of
        False -> Left "Invalid MetaInfo!"
        True -> Right $ MetaInfo val

-- | SHA1 hashing
sha1 :: ByteString -> Digest SHA1
sha1 = hash

-- | Generate a SHA1 info_hash from MetaInfo.
infoHash :: MetaInfo -> ByteString
infoHash (MetaInfo (BDict m)) =
  let i = m ! (key "info")
      s = bEncode i
  in (BS.pack . show . sha1) s

-- | Convenience method. Evaluates to a BStr of the provided String.
key :: String -> BVal
key = BStr . BS.pack

-- | Check whether a BVal is a non-empty BStr.
checkStr :: BVal -> Bool
checkStr (BStr s) = BS.length s > 0
checkStr _ = False

-- | Check whether a BVal is a valid FileDict.
checkFile :: BVal -> Bool
checkFile (BDict f) =
  let l =
        case M.lookup (key "length") f of
          Nothing -> False
          Just (BInt _) -> True
      p =
        case M.lookup (key "path") f of
          Nothing -> False
          Just (BList []) -> False
          Just (BList xs) ->
            let items = map checkStr xs
            in foldr (&&) True items
  in l && p
checkFile _ = False

-- | Check whether a BVal is a valid InfoDict.
checkInfo :: BVal -> Bool
checkInfo (BDict i) =
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
            let items = map checkFile ld
            in foldr (&&) True items
      xor a b = (a || b) && not (a && b)
  in n && pl && p && (xor l f)
checkInfo _ = False

-- | Check whether a BVal is a valid MetaInfo.
checkMeta :: BVal -> Bool
checkMeta (BDict m) =
  let announce =
        case M.lookup (key "announce") m of
          Nothing -> False
          Just (BStr _) -> True
      info =
        case M.lookup (key "info") m of
          Nothing -> False
          Just i -> checkInfo i
  in announce && info
checkMeta _ = False
