{-# LANGUAGE OverloadedStrings #-}
module Greg.IRC where
import Greg.Types
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Network
import System.IO            (hSetBuffering, BufferMode (NoBuffering), hClose)
import Control.Monad        ((>=>))
import Control.Concurrent   (takeMVar)

-- | send raw IRC commands
{-# INLINE send #-}
send :: Bot -> T.Text -> IO ()
send = T.hPutStrLn . socket

connect :: Bot -> IO Bot
connect bot = do
    let conf = config bot
    socket_ <- uncurry connectTo $ network conf
    hSetBuffering socket_ NoBuffering

    -- set nickname, username
    T.hPutStrLn socket_ $ "NICK " `T.append` nickname conf
    T.hPutStrLn socket_ $ "USER " `T.append` username conf `T.append` " 0 * :" `T.append` realname conf

    -- join channel
    T.hPutStrLn socket_ $ "JOIN " `T.append` channel conf

    return (bot { socket = socket_ }) 

disconnect :: Bot -> IO Bot
disconnect bot = do
    qs <- takeMVar (quotes bot)
    ps <- takeMVar (permissions bot)
    writeFile (permFile  (config bot)) (show ps)
    writeFile (quoteFile (config bot)) (show qs)
    hClose (socket bot)
    return bot

reconnect :: Bot -> IO Bot
reconnect = disconnect >=> connect
