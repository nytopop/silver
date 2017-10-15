{- |
Module      :  Network.Silver.Client
Description :  Bittorrent network client.
Copyright   :  (c) Eric Izoita 2017
License     :  BSD3

Maintainer  :  ericizoita@gmail.com
Stability   :  experimental
Portability :  portable
-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Silver.Client
  ( Client(..)
  , mkClient
  ) where

-- Binary Data
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Concurrency
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Monad.STM (atomically)
import System.Random (getStdGen, randoms)

-- Containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

-- Networking
import Network.Socket (SockAddr)

-- Internal
import Network.Silver.Blob (Blob, mkBlob)
import Network.Silver.Meta (MetaInfo)

data Client = Client
  { clientMeta :: MetaInfo
  , clientID :: ByteString
  , clientPort :: Int
  , clientUploaded :: TVar Int
  , clientDownloaded :: TVar Int
  , clientLeft :: TVar Int
  , clientBlob :: Blob
  , clientAvail :: TVar (Map Integer Bool)
  , clientPeers :: TVar (Set SockAddr)
  }

-- Make a new client for a particular meta info dictionary.
-- This function will allocate storage on disk for all component
-- files.
mkClient :: MetaInfo -> Int -> IO Client
mkClient meta port = do
  rng <- getStdGen
  let vals = randoms rng :: [Char]
      myid = BS.concat ["qB", BS.pack $ take 18 vals]
  up <- atomically $ newTVar 0
  down <- atomically $ newTVar 0
  left <- atomically $ newTVar 0
  blob <- mkBlob meta
  avail <- atomically $ newTVar $ M.fromList []
  peers <- atomically $ newTVar $ S.fromList []
  return $
    Client
    { clientMeta = meta
    , clientID = myid
    , clientPort = port
    , clientUploaded = up
    , clientDownloaded = down
    , clientLeft = left
    , clientBlob = blob
    , clientAvail = avail
    , clientPeers = peers
    }
