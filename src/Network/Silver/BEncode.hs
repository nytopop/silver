module Network.Silver.BEncode
  ( pBVal
  ) where

import Control.Applicative ((*>), (<$>), (<*), (<*>), (<|>), pure)
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text (Parser)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

data BVal
  = BInt Integer
  | BStr Text
  | BList [BVal]
  | BDict (M.Map Text BVal)
  deriving (Show, Eq)

-- | Parse a BVal.
pBVal :: Parser BVal
pBVal = pBInt <|> pBStr

-- | Parse a BInt.
pBInt :: Parser BVal
pBInt = BInt <$> (intP *> int <* endP)
  where
    intP = A.string (T.singleton 'i')
    int = toInteger <$> (A.signed A.decimal)
    endP = A.string (T.singleton 'e')

-- | Parse a BStr.
pBStr :: Parser BVal
pBStr = BStr <$> strP
  where
    strP = do
      len <- A.decimal
      A.string (T.singleton ':')
      A.take (fromIntegral len :: Int)

-- | Parse a BList.
-- pBList :: Parser BVal
-- | Parse a BDict.
-- pBDict :: Parser BDict
{-
data LVal
  = LListStart
  | LDictStart
  | LInt Integer
  | LStr String
  | LStop
  deriving (Show, Eq)
-}
-- *************   OLD   *****************
data Symbol
  = ListBegin
  | DictBegin
  | SInt Integer
  | Stop
  | SStr String
  deriving (Show)

-- | Lexer
-- Converts a bencoded string to a list of symbols.
-- TODO : read
tokenize :: String -> [Symbol]
tokenize src =
  case src of
    ('l':rst) -> ListBegin : tokenize rst
    ('d':rst) -> DictBegin : tokenize rst
    xs@('i':_) ->
      let raw = takeWhile (\x -> x /= 'e') xs
          num = read (tail raw) :: Integer -- This might fail
          rst = tail $ drop (length raw) xs -- this might fail
      in SInt num : tokenize rst
    ('e':rst) -> Stop : tokenize rst
    [] -> []
    xs ->
      let rawBuf = takeWhile (\x -> x /= ':') xs
          buf = read rawBuf :: Int -- this might fail
          rawStr = tail $ drop (length rawBuf) xs -- this might fail
          str = take buf rawStr -- this might fail
          rst = drop buf rawStr -- 
      in SStr str : tokenize rst
