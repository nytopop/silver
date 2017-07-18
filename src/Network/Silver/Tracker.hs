{- |
Module      :  Network.Silver.Tracker
Description :  Communication with tracker servers.
Copyright   :  (c) Eric Izoita 2017
License     :  BSD3

Maintainer  :  ericizoita@gmail.com
Stability   :  experimental
Portability :  portable

This module handles communications with bittorrent trackers.
-}
module Network.Silver.Tracker
  (
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Network.Silver.Meta
-- send a get request to a tracker
--
-- GET /announce
-- ?info_hash=J %c9 %c7 %93 %8c %09 f %96 %a2 V H %2f %5b %80 %c8 w q %f2 c R
-- &peer_id=-qB33B0-.0!Z3EFxhyo2
-- &port=27638
-- &uploaded=0
-- &downloaded=745768311
-- &left=835190784
-- &corrupt=0
-- &key=ACEF95F9
-- &event=stopped
-- &numwant=0
-- &compact=1
-- &no_peer_id=1
-- &supportcrypto=1
-- &redundant=0
--
-- RESP
-- d8:completei173e10:incompletei6e8:intervali1746e12:min intervali873e5:peers0:e
