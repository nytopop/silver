module Network.Silver.BEncode
  ( BVal
  , parseBVal
  ) where

import Control.Applicative ((*>), (<$>), (<*), (<*>), (<|>))
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M

-- | A single bencoded value.
data BVal
  = BInt Integer
  | BStr ByteString
  | BList [BVal]
  | BDict (M.Map BVal BVal)
  deriving (Show, Eq, Ord)

{-
instance Binary BVal where
  put _ = put (BS.pack "hello there i'm a fake BVal")
  get = do
    s <- get
    case A.parse parseBVal s of
      A.Fail _ _ _ -> fail "Failed with err"
      A.Partial _ -> fail "Not enough input."
      A.Done _ val -> return val
-}
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
