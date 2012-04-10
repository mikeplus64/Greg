{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as T
import qualified Data.Text    as T

import Control.Concurrent
import Control.Monad

import Network

import IRC
import Bot
import Types

myConnection ::  BotConfig
myConnection = BC {
    nickname  = "GREG-THE-WARRIOR",
    username  = "GERG",
    realname  = "CONAN",
    channel   = "#testing135",
    network   = ("irc.rizon.net", PortNumber 6667),
    commands  = defaultCommands,
    quoteFile = "/home/opuk/.greg/quotes",
    permFile  = "/home/opuk/.greg/permissions"
}

main :: IO ()
main = do
    bot <- connect myConnection

    -- extremely simple IRC client
    forkIO $ forever $ do
        line <- T.getLine
        case line of
            "/quit" -> void $ disconnect bot
            "/QUIT" -> void $ disconnect bot
            _ -> case ("/" `T.isPrefixOf` line, "//" `T.isPrefixOf` line) of
                (True,  _) -> send    bot (T.drop 1 line)
                (_,  True) -> message bot (T.drop 1 line)
                _          -> message bot line

    forever $ do
        line <- T.hGetLine (socket bot)
        T.putStrLn line
        
        case T.take 4 line of
            "PING" -> send bot $ "PONG " `T.append` T.drop 5 line
            _      -> case parseMessage line bot of
                Just mg ->
                    case parseCommand mg (commands (config bot)) of
                        Just (command, args) -> msgCommand bot mg{ msg = args } command
                        _                    -> return ()
                _ -> return ()
