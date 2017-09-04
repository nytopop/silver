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
--
import qualified Data.ByteString.Char8 as BS
import Network.Silver.Meta (decodeMeta)
import Network.Silver.Torrent (newClient, runClient)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  bs <- BS.readFile $ head args
  let (Just meta) = decodeMeta bs
  newClient meta 8199 >>= runClient
