{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

module System.RedProx.Core (
    module Database.Redis,
    proxy,
    createSession,
    createWorld,
    stopWorld,
    killWorld,
) where

import System.RedProx.Types
import System.RedProx.Misc
import System.RedProx.Poison

import Control.Exception
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Loops
import Database.Redis
import Data.Monoid
import qualified Data.ByteString.Char8 as BS

-- | Create a "world" which consists of sessions between various redis servers & threads which proxy payloads (queues, pubsub) between them.
createWorld :: [ProxyDef] -> IO World
createWorld proxies = do
    sessions <- mapM createSession proxies
    return $ World {
        _session = sessions
    }

-- | Stop a world by poisoning every session.
stopWorld :: World -> IO ()
stopWorld World{..} = do
    mapM_ (\session -> poison (_sessionPref session)) _session

-- | Kill a world by forcefully killing all proxy ThreadId's within each session.
killWorld :: World -> IO ()
killWorld w@World{..} = do
    stopWorld w
    mapM_ (\tid -> throwTo tid DeathException) $ concat $ map (\session -> _sessionThreads session) _session

-- | Create a session.
-- | A session consists of:
-- | * Tunnels between several redis servers.
-- | * A thread which proxies information from a source to a target.
-- | * ProxyDef declares what we want to proxy; queues and/or pubsub messages.
-- | * A "transformer" which gives the implementer the ability to modify the key and/or body data of each payload.
-- | * A persistence variable, used to retry a proxy in the event that we failed to send it over to the target.
-- | * A poison channel that gives one the ability to gracefully exit all proxy threads.
-- | * A ThreadId list so that we can forcefully kill all proxy threads.
createSession :: ProxyDef -> IO Session
createSession ProxyDef{..} = do
    pref <- newPoisonRef
    let session = Session {
        _sessionName = _proxyName,
        _sessionPref = pref,
        _sessionThreads = []
    }
    sources <- mapM
        (\(source, prx, target) -> do
            connectEndpoints session source prx target
        ) [ (source, prx, target) | source <- _proxySources, target <- _proxyTargets, prx <- _proxy]
    return $ session {
        _sessionThreads = getThreadsFromSessionSources sources
    }
    where
        getThreadsFromSessionSources ss = map (\(tid, _) -> tid) ss

-- | Connect a single source to a target. Repeat indefinitely.
connectEndpoints :: Session -> SourceConnectInfo -> Proxy -> TargetConnectInfo -> IO SessionSource
connectEndpoints session@Session{..} source prx target = do
    tid <- forkIO $ do
        pvar <- newEmptyMVar
        let thread = Thread {
            _threadPersistVar = pvar,
            _threadSourceCI = source,
            _threadTargetCI = target,
            _threadProxy = prx
        }
        whileM_ (isPoisoned _sessionPref >>= \b -> notM b) $ connectEndpoints' session thread >> delay
    return (tid, prx)

-- | Connect a single source to a target. Make sure we catch several exceptions.
connectEndpoints' :: Session -> Thread -> IO ()
connectEndpoints' session@Session{..} thread@Thread{..} = do
    catches
        (connectEndpoints'' session thread)
        [Handler someExceptionHandler, Handler redisExceptionHandler, Handler redproxExceptionHandler]
    where
        putErr e = putStrLn $ "connectEndpoints: " ++ show e
        someExceptionHandler :: SomeException -> IO ()
        someExceptionHandler e = putErr e
        redisExceptionHandler :: ConnectionLostException -> IO ()
        redisExceptionHandler e = putErr e
        redproxExceptionHandler :: RedProxException -> IO ()
        redproxExceptionHandler e@(DeathException) = putErr e
        redproxExceptionHandler e = putErr e

-- | Connect a single source to a target. Make sure we cleanup each redis connection if an error occurs.
connectEndpoints'' :: Session -> Thread -> IO ()
connectEndpoints'' session@Session{..} thread@Thread{..} = do
    bracket
        (do connect _threadSourceCI)
        (\source' -> runRedis source' $ quit)
        (\source' ->
            bracket
                (connect _threadTargetCI >>= \target' -> return target')
                (\target' -> (runRedis source' $ quit) >> (runRedis target' $ quit))
                (\target' -> proxy session $ thread { _threadSource = source', _threadTarget = target' })
        )

-- | Proxy payloads from source to target.
proxy :: Session -> Thread -> IO ()
proxy session@Session{..} thread@Thread{..} = do
    empty <- isEmptyMVar _threadPersistVar
    if empty
        then (consumer $ ins _threadProxy) session thread
        else do
            out <- takeMVar _threadPersistVar
            proxyOut session thread out

-- | Send the final payload (key/value) on to the target redis server.
proxyOut :: Session -> Thread -> Out -> IO ()
proxyOut Session{..} Thread{..} out = do
    bracketOnError
        ((case out of
                LPush (Payload k v) -> runRedis _threadTarget $ lpush k [v]
                RPush (Payload k v) -> runRedis _threadTarget $ rpush k [v]
                Publish (Payload k v) -> runRedis _threadTarget $ publish k v
                NoOp (Payload _ _) -> return $ Right 0
            ))
        (\_ -> putMVar _threadPersistVar out)
        (\r -> either (\_ -> throw TargetException) (\_ -> return ()) r)

-- |
genericPop :: Session -> Thread -> IO ()
genericPop session@Session{..} thread@Thread{..} = do
    whileM_ (isPoisoned _sessionPref >>= \b -> notM b) $ genericPop' session thread

-- |
genericPop' :: Session -> Thread -> IO ()
genericPop' session@Session{..} thread@Thread{..} = do
    r <- runRedis _threadSource $ do
        let (p, s) = popper $ ins _threadProxy
        p s 30
    either
        (\_ -> throw SourceException)
        (\result -> maybe (return ()) (\(k, v) -> mapM_ (\trfm -> proxyOut session thread (trfm k v)) (outs _threadProxy)) result)
        r

-- |
genericSub :: Session -> Thread -> IO ()
genericSub session@Session{..} thread@Thread{..} = do
    runRedis _threadSource $ do
        let (sub, unsub, s) = subber $ ins _threadProxy
        pubSub (sub s) $ \msg -> do
            let
                k = msgChannel msg
                v = msgMessage msg
            liftIO $ mapM_ (\trfm -> proxyOut session thread (trfm k v)) (outs _threadProxy)
            poisoned <- liftIO $ isPoisoned _sessionPref
            if (not poisoned)
                then return mempty
                else return $ unsub s

-- |
consumer :: In -> (Session -> Thread -> IO ())
consumer (BLPop _) = genericPop
consumer (BRPop _) = genericPop
consumer (Sub _) = genericSub
consumer (PSub _) = genericSub

-- |
popper :: RedisCtx m f => In -> ([BS.ByteString] -> Integer -> m (f (Maybe (BS.ByteString, BS.ByteString))), [Key])
popper (BLPop s) = (blpop, s)
popper (BRPop s) = (brpop, s)
popper _ = error "popper pattern match failure."

-- |
subber :: In -> ([BS.ByteString] -> PubSub, [BS.ByteString] -> PubSub, [Key])
subber (Sub s) = (subscribe, unsubscribe, s)
subber (PSub s) = (psubscribe, punsubscribe, s)
subber _ = error "subber pattern match failure."

-- |
ins :: (a, b) -> a
ins = fst

-- |
outs :: (a, b) -> b
outs = snd
