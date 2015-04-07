{-# LANGUAGE OverloadedStrings #-}

import System.RedProx
import System.Exit
import Control.Monad
import qualified Data.ByteString.Char8 as BS

t1 :: ProxyDef
t1 = ProxyDef "local1" [defaultConnectInfo] [defaultConnectInfo] [p1,p2,p3]
    where
        p1 = (BLPop ["proxy1", "proxy2"],
            [
                (\key body -> RPush (Payload { _key = (BS.concat ["proxy:blpop:rpush:", key]), _body = body })),
                (\key body -> LPush (Payload { _key = (BS.concat ["proxy:blpop:lpush:", key]), _body = body })),
                (\key body -> Publish (Payload { _key = (BS.concat ["proxy:blpop:publish:", key]), _body = body }))
            ])
        p2 = (Sub ["sub"], [(\key body -> LPush (Payload { _key = (BS.concat ["proxy:sub", key]), _body = body }))])
        p3 = (PSub ["psub:*"], [(\key body -> Publish (Payload { _key = (BS.concat ["proxy:psub:", key]), _body = body }))])

t2 :: ProxyDef
t2 = ProxyDef "local2" [defaultConnectInfo] [defaultConnectInfo] [p1]
    where
        p1 = (BRPop ["proxy1", "proxy2"],
            [
                (\key body -> RPush (Payload { _key = (BS.concat ["proxy:brpop:rpush:", key]), _body = body })),
                (\key body -> LPush (Payload { _key = (BS.concat ["proxy:brpop:lpush:", key]), _body = body })),
                (\key body -> Publish (Payload { _key = (BS.concat ["proxy:brpop:publish:", key]), _body = body }))
            ])

t3 :: ProxyDef
t3 = ProxyDef {
        _proxyName = "local3",
        _proxySources = [defaultConnectInfo],
        _proxyTargets =  [defaultConnectInfo],
        _proxy = [p1]
    }
    where
        p1 = (BLPop [ BS.concat ["proxy", BS.pack $ show n] | n <- [1..100] :: [Integer]],
            [(\key body -> Publish (Payload { _key = (BS.concat ["proxy:brpop:publish:", key]), _body = body }))])

t4 :: ProxyDef
t4 = ProxyDef {
        _proxyName = "local4",
        _proxySources = [defaultConnectInfo],
        _proxyTargets =  [defaultConnectInfo],
        _proxy = [p1]
    }
    where
        p1 = (BLPop ["proxy:persist"],
            [(\key body -> RPush (Payload { _key = (BS.concat ["proxy:test:", key]), _body = body }))])

proxies :: [ProxyDef]
proxies = [t1,t2,t3,t4]

main :: IO ()
main = do
    w <- createWorld proxies
    forever $
        getLine >>=
        (\line -> case line of
            [] -> return ()
            ('q':_) -> stopWorld w >> putStrLn "stopping..."
            ('k':_) -> killWorld w >> putStrLn "killing..."
            ('e':_) -> putStrLn "exiting.." >> exitSuccess
            _ -> return ())
