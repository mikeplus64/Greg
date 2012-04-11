{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Greg.Simple (createBot, client, monitor) where
import qualified Data.Text    as T (Text, append, isPrefixOf, take, drop, unpack)
import qualified Data.Text.IO as T (hGetLine, getLine, putStrLn, readFile)
import Control.Concurrent  (newMVar)
import Control.Applicative ((<$>))
import Control.Monad       (forever, void)
import Data.Map            (empty, Map)
import Data.IntMap         (IntMap)

import Greg.Parse
import Greg.IRC
import Greg.Bot
import Greg.Types

{-
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
-}

createBot :: String -> [Command] -> Command -> IO Bot
createBot path commandList msger = do
    conf <- configure path

    qf <- T.unpack <$> T.readFile (quoteFile conf) -- quotes file
    pf <- T.unpack <$> T.readFile (permFile  conf) -- permissions file

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
            commands    = commandList,
            msgHandler  = msger,
            config      = conf
        }


client :: Bot -> IO ()
client bot = forever $ do
    line <- T.getLine
    case line of
        "/quit" -> void $ disconnect bot
        "/QUIT" -> void $ disconnect bot
        _ -> case ("/" `T.isPrefixOf` line, "//" `T.isPrefixOf` line) of
            (True,  _) -> send    bot (T.drop 1 line)
            (_,  True) -> message bot (T.drop 1 line)
            _          -> message bot line

monitor :: Bot -> IO ()
monitor bot = forever $ do
    line <- T.hGetLine (socket bot)
    T.putStrLn line

    case T.take 4 line of
        "PING" -> send bot $ "PONG " `T.append` T.drop 5 line
        _      -> case parseMessage line bot of
            Just mg ->
                case parseCommand mg (commands  bot) of
                    Just (command, args) -> msgCommand bot mg{ msg = args } command
                    _                    -> msgCommand bot mg (msgHandler bot)
            _ -> return ()
