{-# LANGUAGE OverloadedStrings #-}
module IRC where
import Types
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Network
import Data.Map as M        (empty)
import System.IO            (hSetBuffering, BufferMode (NoBuffering), hClose)
import Control.Concurrent   (newMVar)
import Control.Monad        ((>=>))

send :: Bot -> T.Text -> IO ()
send = T.hPutStrLn . socket

connect :: BotConfig -> IO Bot
connect details = do
    socket_ <- uncurry connectTo $ network details
    hSetBuffering socket_ NoBuffering

    -- set nickname, username
    T.hPutStrLn socket_ $ "NICK " `T.append` nickname details
    T.hPutStrLn socket_ $ "USER " `T.append` username details `T.append` " 0 * :" `T.append` realname details

    -- join channel
    T.hPutStrLn socket_ $ "JOIN " `T.append` channel details

    qs <- newMVar M.empty
    ps <- newMVar M.empty

    return Bot {
            quotes      = qs,
            permissions = ps,
            socket      = socket_,
            config      = details
        }

disconnect :: Bot -> IO Bot
disconnect bot = do
    T.hPutStrLn (socket bot) "QUIT"
    hClose (socket bot)
    return bot

reconnect :: Bot -> IO Bot
reconnect = disconnect >=> connect . config
