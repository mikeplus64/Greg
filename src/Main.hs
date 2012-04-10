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
    nickname = "GREG-THE-WARRIOR",
    username = "GERG",
    realname = "CONAN",
    channel  = "#merc-devel",
    network  = ("irc.freenode.net", PortNumber 6667),
    commands = defaultCommands
}

main :: IO ()
main = do
    bot <- connect myConnection

    -- extremely simple IRC client
    forkIO $ forever $ do
        line <- T.getLine
        case ("/" `T.isPrefixOf` line, "//" `T.isPrefixOf` line) of
            (_,  True) -> message bot (T.drop 1 line)
            (True,  _) -> send    bot (T.drop 1 line)
            _          -> message bot line

    forever $ do
        line <- T.hGetLine (socket bot)
        T.putStrLn line
        case parseMessage line bot of
            Just ms@(Msg s _) ->
                case parseCommand ms (commands (config bot)) of
                    Just (command, args) -> msgCommand (command, s, args) bot
                    _                    -> addToQuotes bot ms
            _ -> return ()
