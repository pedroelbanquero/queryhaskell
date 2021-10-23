module Network where

import Data.Monoid ((<>))

import Data.Char ( chr )


import Control.Concurrent 
import qualified Control.Exception as E
import Control.Monad 
import qualified Data.ByteString as S
import Network.Socket
import qualified Network.Socket.ByteString as NSB

import qualified Data.ByteString.Char8 as C

import Data.Bits
import qualified Data.ByteString as BS

import System.IO
import System.Environment
import Control.Exception
import qualified Control.Exception.Safe as SA

import qualified Data.ByteString.Lazy.Char8 as Char8

import qualified Data.List.Split as S2

import Control.Concurrent.STM

import Control.Parallel.Strategies

import System.Directory


import Data.ByteString (ByteString)



import Data.Either

import Query

cobaltoracle port url_out args = do
    hSetBuffering stdout NoBuffering            
    forkIO $ startCO port url_out args

parsebod dat = bodparsed
	where
	bod = map (\x-> S2.splitOn "=" x) $ S2.splitOn "&" $ last dat
	bodparsed = map (\[x,y]-> (x,y)) bod

startCO port url_out args= withSocketsDo $ do
    print $ "Cluster Proxy"
    addr <- resolve port
    E.bracket (open addr) close loop
    where
    resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
                , addrSocketType = Stream
                }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        -- If the prefork technique is not used,
        -- set CloseOnExec for the security reasons.
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock (addrAddress addr)
        listen sock 10000
        return sock

    loop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (talk conn peer) (\_ -> close conn)

    talk conn peer= do
        let p = show peer
        let hpeer= S2.wordsBy (==':') (p)
        
        msg <- NSB.recv conn 2000800
       
        let ms = C.unpack msg
        print ms 
        unless (S.null msg) $ do
          
            let dat = S2.splitOn "\r\n\r\n" ms 
	    print "Request :"
   
	    let meth = head $ S2.splitOn " " (head (S2.splitOn "\r\n" (dat !! 0)))
	    let path = (S2.splitOn " " (head (S2.splitOn "\r\n" (dat !! 0)))) !! 1
	    
	    (heds,dats) <- urlCall (url_out++path) ms meth 
            print "Response :"
	    print $ length dats
            
            let default_response = "HTTP/1.1 403 Forbidden\r\nCache-Control: no-cache\r\nConnection: close\r\nContent-Type: text/html\r\n\r\nForbidden\r\n"
    	    print heds 
            let ok_response = "HTTP/1.1 200 OK\r\ncache-control: no-cache\r\ncontent-type: "++heds++"\r\n\r\n"
	    	

	    NSB.sendAll conn (C.pack (concat ([ok_response]++[dats])))

            --talk conn peer



talk sock g= do


    res <- try (NSB.sendAll sock $ g)
    let sock2 = (case res of
            Left  e    -> show (e :: IOException)
            Right response -> show "1")


    let len = len
    return $ res
