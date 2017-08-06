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
  , decodeMetaFile
  , decodeMeta
  , isFileDict
  , isInfoDict
  , isMetaInfo
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))
import Network.Silver.BEncode (BVal(..), bDecode, bEncode, key)
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
