{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import           Data.Maybe
import qualified Data.Text.Encoding as T
import		 Data.Text hiding (index,dropWhile)
import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist
import           Snap.Extension
import Control.Monad.Trans (liftIO)

import           Application

import Webparser
------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices =
        [ ("start-time",   startTimeSplice)
        , ("current-time", currentTimeSplice)
        ]


------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    abc <- getParam "hij"
    heistLocal (bindString "message" (T.decodeUtf8 message)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | Renders the parser page.
parser :: Application ()
parser = do
    code <- decodedParam "code"
    result <- liftIO $runOne $decode code
    heistLocal (bindString "result" (pack ((decode code) ++ "\n"++ result ++ "\n" ))) $ render "scheme"
  where
    decodedParam p = fromMaybe "(+ 2 3)" <$> getParam p
    decode =  dropWhile (==' ') . unpack . T.decodeUtf8 

schemer :: Application ()
schemer = do
    code <- decodedParam "code"
    result <- liftIO $runOne $decode code
    heistLocal (bindString "result" (pack ((decode code) ++ "\n"++ result ++ "\n" ))) $ render "schemer"
  where
    decodedParam p = fromMaybe "(+ 2 3)" <$> getParam p
    decode =  dropWhile (==' ') . unpack . T.decodeUtf8 



eval2json :: Application ()
eval2json = do
    code <- decodedParam "code"
    result <- liftIO $runOne $decode code
    heistLocal (bindString "result" (pack result )) $ render "json"
    --heistLocal (bindString "result" (pack ((decode code) ++ "\n"++ result ++ "\n" ))) $ render "json"
  where
    decodedParam p = fromMaybe "(+ 2 3)" <$> getParam p
    decode =  dropWhile (==' ') . unpack . T.decodeUtf8 




------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = route [ ("/",            index)
             , ("/echo/:stuff", echo)
             , ("/parser/:code", parser)
             , ("/eval2json/:code", eval2json)
             , ("/schemer/:code", schemer)
             ]
       <|> serveDirectory "resources/static"
