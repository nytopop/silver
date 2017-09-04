{- |
Module      :  Network.Silver.BEncode
Description :  Bencoding serialization & deserialization.
Copyright   :  (c) Eric Izoita 2017
License     :  BSD3

Maintainer  :  ericizoita@gmail.com
Stability   :  experimental
Portability :  portable

This module handles the conversion of bencoded ByteStrings 
to BVals, and conversion of BVals to bencoded ByteStrings. 

Parsers use Attoparsec.
-}
module Network.Silver.BEncode
  ( BVal(..)
  , bEncode
  , bDecode
  , key
  ) where

import Control.Applicative ((*>), (<$>), (<*), (<*>), (<|>))
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

-- | A single bencoded value.
data BVal
  = BInt Integer
  | BStr ByteString
  | BList [BVal]
  | BDict (Map BVal BVal)
  deriving (Show, Eq, Ord)

-- | Convenience function to pack string as BStr.
key :: String -> BVal
key = BStr . BS.pack

-- | Encode a BVal into a ByteString.
bEncode :: BVal -> ByteString
bEncode (BInt i) =
  let prefix = BS.singleton 'i'
      midfix = BS.pack (show i)
      suffix = BS.singleton 'e'
  in BS.concat [prefix, midfix, suffix]
bEncode (BStr x) =
  let midfix = BS.singleton ':'
      prefix = BS.pack (show $ BS.length x)
  in BS.concat [prefix, midfix, x]
bEncode (BList xs) =
  let prefix = BS.singleton 'l'
      midfix = BS.concat $ map bEncode xs
      suffix = BS.singleton 'e'
  in BS.concat [prefix, midfix, suffix]
bEncode (BDict ds) =
  let xs = M.toAscList ds
      fun (k, v) = BS.concat [bEncode k, bEncode v]
      midfix = BS.concat $ map fun xs
      prefix = BS.singleton 'd'
      suffix = BS.singleton 'e'
  in BS.concat [prefix, midfix, suffix]

-- | Decode a BVal from a ByteString.
bDecode :: ByteString -> Maybe BVal
bDecode xs =
  case A.parseOnly bVal xs of
    Left msg -> Just $ BStr $ BS.pack msg
    Right val -> Just val

-- | Parse a BVal.
bVal :: Parser BVal
bVal = bInt <|> bStr <|> bList <|> bDict

-- | Parse a BInt.
bInt :: Parser BVal
bInt = do
  A.char 'i'
  sign <- A.option ' ' (A.char '-')
  val <- A.many1 A.digit
  A.char 'e'
  mkInt sign val
  where
    mkInt ' ' ['0'] = return $ BInt 0
    mkInt '-' ('0':rst) = fail "Negative zero!"
    mkInt _ ('0':rst) = fail "Leading zero(s)!"
    mkInt '-' nums = return $ BInt $ read ('-' : nums)
    mkInt _ nums = return $ BInt $ read nums

-- | Parse a BStr.
-- TODO : disallow leading zeroes
bStr :: Parser BVal
bStr =
  let bufP = A.decimal <* A.char ':'
      strP = do
        len <- bufP
        A.take (fromIntegral len :: Int)
  in BStr <$> strP

-- | Parse a BList.
bList :: Parser BVal
bList =
  let listP = A.char 'l' *> A.many1 bVal <* A.char 'e'
  in BList <$> listP

-- | Parse a BDict.
-- TODO : ensure keys are sorted
bDict :: Parser BVal
bDict =
  let ascP = A.char 'd' *> A.many1 dictP <* A.char 'e'
      dictP = (\a b -> (a, b)) <$> bStr <*> bVal
  in BDict <$> M.fromList <$> ascP
