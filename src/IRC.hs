{-# LANGUAGE OverloadedStrings #-}
module IRC where
import Types
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Network
import Data.Map             (Map, empty)
import Data.IntMap          (IntMap)
import System.IO            (hSetBuffering, BufferMode (NoBuffering), hClose)
import Control.Concurrent   (newMVar, takeMVar)
import Control.Monad        ((>=>))
import Control.Applicative  ((<$>))

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

    qf <- T.unpack <$> T.readFile (quoteFile details) -- quotes file
    pf <- T.unpack <$> T.readFile (permFile details)  -- permissions file

    let parse !f = 
            let parsed = reads f
            in if null parsed
                then empty
                else fst (head parsed) 

        !quotes_  = parse qf :: Map T.Text (IntMap T.Text)
        !permits_ = parse pf :: Map T.Text Permission

    qs <- newMVar quotes_
    ps <- newMVar permits_

    return Bot {
            quotes      = qs,
            permissions = ps,
            socket      = socket_,
            config      = details
        }

disconnect :: Bot -> IO Bot
disconnect bot = do
    qs <- takeMVar (quotes bot)
    ps <- takeMVar (permissions bot)
    writeFile (permFile  (config bot)) (show ps)
    writeFile (quoteFile (config bot)) (show qs)
    T.hPutStrLn (socket bot) "QUIT"
    hClose (socket bot)
    return bot

reconnect :: Bot -> IO Bot
reconnect = disconnect >=> connect . config
