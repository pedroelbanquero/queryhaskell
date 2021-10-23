{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE PackageImports #-}

module Query where

import Text.XML.HXT.Core
import Text.HTML.Scalpel as SC
import Control.Applicative
import Control.Monad
import Data.List (isInfixOf)

import Network.HTTP 
import qualified Network.HTTP.Headers as HE
--import qualified Network.Http.Types as NT
import Network.HTTP.Client 
import Network.HTTP.Client.TLS 
import qualified Network.HTTP.Types.Header as HT 
import Network.HTTP.Types.Status 
--import Network.Simple.TCP.TLS
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Conduit as CO

import qualified Network.Connection as CON

import qualified Data.List.Split as DS

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Tree.NTree.TypeDefs
import Data.Maybe

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Network.URI
import System.Environment
import qualified Data.Text as T

import System.Process
import Network.Socket
import Control.Concurrent 
import qualified Control.Monad.Except as Exc
import           Control.Exception          (try)
import qualified Network.HTTP.Simple as NHS
import Control.Exception.Extra


import Data.Default

import qualified Data.List as DL
import Data.Maybe
import qualified Data.Map as M
import GHC.Generics
import Data.Text (Text)

import Control.Applicative ((<$>))

import Control.Concurrent.Async

import qualified Data.ByteString.Lazy.Char8 as Char8

import Text.Regex (mkRegex, subRegex)


linksFrom url = scrapeURL url catComment
 where
  catComment :: SC.Scraper String [(String)]
  catComment =

    chroots (tagSelector "a") $ do
      altText <- SC.attr "href" anySelector
      return (altText)


imagesFrom a url = scrapeURL url catComment
 where
  catComment :: SC.Scraper String [(String)]
  catComment =

    chroots (tagSelector "img") $ do
      altText <- SC.attr "src" anySelector
      return (altText)


getFromOne a b url = scrapeURL url (catComment)
 where
  catComment :: SC.Scraper String [(String)]
  catComment =

    chroots (tagSelector a) $ do
      altText <- SC.attr (b) anySelector
      return (altText)

downloadFrom url = do
    images <- scrapeURL url (htmls anySelector)
    maybe printError printImages images
    where
        printError = putStrLn "ERROR: Could not scrape the URL!"
        printImages = mapM_ putStrLn

getFrom sels url dat = do 

  let newsels= tail sels
  let a = fst (head sels)
  let b = snd (head sels)
  let len = length sels
  fromsel <- getFromOne a b url
  let out = dat ++ [fromJust fromsel]
  
  case (len) of
       1 -> return $ out
       otherwise -> getFrom newsels url out


parseheader d = (concat $ reverse $ tail $ reverse $ (tail $ (DS.splitOn "\"" (show d))))::String


parsehead d =  (map (\[e,r]-> (e,r)) $ map (\b-> DS.splitOn ": " b)   $ tail $ DS.splitOn "\r\n" d )

parsehead2 d =  (map (\[e,r]-> (e, r)) $ map (\b-> DS.splitOn ": " b)   $ tail $ DS.splitOn "\r\n" d )

bines n = ""

getip = ""




replace a b c= subRegex (mkRegex a) c b



urlCall url dat meth = do
    print "URL CALL"
    print url

    print dat

    initialRequest <- NHS.parseRequest url 
    
    let separated = (DS.splitOn "\r\n\r\n" (dat) )
    let heads = separated !! 0
    let bod = separated !! 1
    let headsparsed  = tail  $ parsehead heads 
    print $ headsparsed 

    --let host =  head $ DS.splitOn "/" $ (DS.splitOn "https://" url) !! 1
    print "HOST"
    --print host


    -- Manipulate headers before the request
    let prov = [("accept","text/html"),("referer","https://wise.com"),("cache-control","no-store")]



    let request = initialRequest 
            { method = (B.pack meth)
	    , requestBody = RequestBodyLBS $ (Char8.pack bod) 
            --, requestHeaders = prov 
            , requestHeaders = (prov++ map (\(x,y) -> (CI.mk (B.pack (x)), B.pack (y))) (filter (\(r,e)-> r/="accept-encoding" && r/="accept" && r/="referer") headsparsed)) 
            }
    

    print request
    response <-  NHS.httpLBS request 
    print response
    --return $  [L8.unpack $ responseHeaders response]++["\r\n\r\n"]++[L8.unpack $ responseBody response]++["\r\n"]
    let ad =  NHS.getResponseHeader "Content-Type" response
    let result = ((B.unpack (head ad), replace "TransferWise is now Wise" "Lescovex / Wise Partner Membership"  $ replace "https://wise.com/public-resources/assets/logos/wise/brand_logo_inverse.svg\"" "https://lescovex.com/wise/logo-lexcovex-wise.svg\" width=\"250px\" " $ L8.unpack (responseBody response)))
    return result

getTextFrom a url = scrapeURL url (catComment)
  where
    catComment :: SC.Scraper String [(String)]
    catComment =

      chroots (tagSelector a) $ do
        altText <- text anySelector
        return (altText)



