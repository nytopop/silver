module Main where

-- | dltor <meta-info file> [OPTIONS]
--    -s, --seed
--     ; seed after completion
--    -g, --greedy
--     ; enable greedy mode
--    -p, --progress
--     ; show progress
--    -v, --verbose  
--     ; enable verbose mode
--    -o OUTPUT, --output OUTPUT
--     ; set output directory (default is '.')
--    -h, --help
--     ; show this message
--
import qualified Data.ByteString.Char8 as BS
import Network.Silver.Client (mkClient)
import Network.Silver.Meta (decodeMeta)
import Network.Silver.Torrent (runClient)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  src <- BS.readFile $ head args
  case decodeMeta src of
    Just meta -> do
      mkClient meta 8199 >>= runClient
    Nothing -> do
      print "meta decodes as Nothing"
