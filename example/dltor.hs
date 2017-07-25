module Main where

-- | dltor <meta-info file> [OPTIONS]
--     OPTIONS
--     -s, --seed     : seed after completion
--     -g, --greedy   : enable greedy mode
--     -p, --progress : show progress
--     -v, --verbose  : enable verbose mode
--     -o OUTPUT      : set output directory (default is '.')
--     -h, --help     : show this message
--
-- read torrent file
-- decode metainfo
-- call mkTorrent
-- call dlTorrent <|>
--      dlsTorrent
--
main :: IO
main = do
  print 0
