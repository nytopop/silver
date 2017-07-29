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
-- read torrent file
-- decode metainfo
-- call mkTorrent
-- call dlTorrent <|>
--      dlsTorrent
--
import qualified Data.ByteString.Char8 as BS
import Network.Silver.Meta (decodeMeta)
import Network.Silver.Torrent (dlTorrent)
import System.Environment

main :: IO ()
main = do
  bs <- BS.readFile "ref/deb.torrent"
  let f (Right x) = x
      minf = f $ decodeMeta bs
  dlTorrent minf
