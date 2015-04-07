{-# LANGUAGE DeriveDataTypeable #-}

module System.RedProx.Types (
    World (..),
    Session (..),
    Thread (..),
    ProxyDef (..),
    Payload (..),
    In (..),
    Out (..),
    Proxy,
    Key,
    Body,
    SessionSource,
    Transformer,
    SourceConnectInfo,
    TargetConnectInfo,
    Source,
    Target,
    RedProxException(..)
) where

import System.RedProx.Poison
import Control.Exception
import Control.Concurrent
import Database.Redis
import Data.Typeable hiding (Proxy)
import qualified Data.ByteString.Char8 as BS

type Key = BS.ByteString
type Body = BS.ByteString
type Transformer = Key -> Body -> Out
type Proxy = (In, [Transformer])
type SessionSource = (ThreadId, Proxy)
type SourceConnectInfo = ConnectInfo
type TargetConnectInfo = ConnectInfo
type Source = Connection
type Target = Connection

data RedProxException = SourceException | TargetException | DeathException deriving (Show, Typeable)
instance Exception RedProxException

data World = World {
    _session :: [Session]
}

data Session = Session {
    _sessionName :: String,
    _sessionPref :: PoisonRef,
    _sessionThreads :: [ThreadId]
}

data Thread = Thread {
    _threadPersistVar :: MVar Out,
    _threadSourceCI :: ConnectInfo,
    _threadSource :: Connection,
    _threadTargetCI :: ConnectInfo,
    _threadTarget :: Connection,
    _threadProxy :: Proxy
}

data ProxyDef = ProxyDef {
    _proxyName :: String,
    _proxySources :: [ConnectInfo],
    _proxyTargets :: [ConnectInfo],
    _proxy :: [Proxy]
}

data Payload = Payload {
    _key :: BS.ByteString,
    _body :: BS.ByteString
}

data In =
      BLPop [Key]
    | BRPop [Key]
    | Sub [Key]
    | PSub [Key]

data Out =
      LPush Payload
    | RPush Payload
    | Publish Payload
    | NoOp Payload
