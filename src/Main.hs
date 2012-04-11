{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as T
import qualified Data.Text    as T

import Control.Concurrent
import Control.Monad
import System.Environment (getArgs)

import Parse
import IRC
import Bot
import Types

main :: IO ()
main = do
    arg  <- getArgs
    conf <- configure (if null arg then "/home/opuk/.greg/config" else head arg)
    bot  <- connect (conf { commands = defaultCommands } )

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
                        _                    -> addToQuotes bot mg
                _ -> return ()
