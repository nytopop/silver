module Network.Silver.BEncode
  ( BVal
  , parseBVal
  , packBVal
  ) where

import Control.Applicative ((*>), (<$>), (<*), (<*>), (<|>))
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Binary
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as M

-- | A single bencoded value.
data BVal
  = BInt Integer
  | BStr ByteString
  | BList [BVal]
  | BDict (M.Map BVal BVal)
  deriving (Show, Eq, Ord)

instance Binary BVal where
  put e = put (BS.concat $ BL.toChunks $ packBVal e)
  get = do
    xs <- get
    case A.parseOnly parseBVal xs of
      Left msg -> fail msg
      Right val -> return val


-- | Parse a BVal.
parseBVal :: Parser BVal
parseBVal =
  parseBInt <|> parseBStr <|> parseBList <|> parseBDict

-- | Parse a BInt.
parseBInt :: Parser BVal
parseBInt = BInt <$> (intP *> int <* endP)
  where
    intP = A.string (BS.singleton 'i')
    int = toInteger <$> (A.signed A.decimal)
    endP = A.string (BS.singleton 'e')

-- | Parse a BStr.
parseBStr :: Parser BVal
parseBStr =
  BStr <$> do
    len <- A.decimal
    A.string (BS.singleton ':')
    A.take (fromIntegral len :: Int)

-- | Parse a BList.
parseBList :: Parser BVal
parseBList =
  BList <$> do
    A.string (BS.singleton 'l')
    list <- A.many1 parseBVal
    A.string (BS.singleton 'e')
    return list

-- | Parse a BDict.
parseBDict :: Parser BVal
parseBDict = BDict <$> M.fromList <$> ascP
  where
    ascP = do
      A.string (BS.singleton 'd')
      list <- A.many1 dictP
      A.string (BS.singleton 'e')
      return list
    dictP = do
      key <- parseBStr
      val <- parseBVal
      return (key, val)

-- | Pack a BVal into a lazy ByteString.
packBVal :: BVal -> BL.ByteString
packBVal (BInt i) =
  let prefix = BL.singleton 'i'
      midfix = BL.pack (show i)
      suffix = BL.singleton 'e'
  in BL.concat [prefix, midfix, suffix]
packBVal (BStr x) =
  let midfix = BL.singleton ':'
      suffix = BL.fromStrict x
      prefix = BL.pack (show $ BL.length suffix)
  in BL.concat [prefix, midfix, suffix]
packBVal (BList xs) =
  let prefix = BL.singleton 'l'
      midfix = BL.concat $ map packBVal xs
      suffix = BL.singleton 'e'
  in BL.concat [prefix, midfix, suffix]
packBVal (BDict asc) =
  let xs = M.toAscList asc
      fun (k, v) = BL.concat [packBVal k, packBVal v]
      midfix = BL.concat $ map fun xs
      prefix = BL.singleton 'd'
      suffix = BL.singleton 'e'
  in BL.concat [prefix, midfix, suffix]
