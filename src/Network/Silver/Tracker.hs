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
