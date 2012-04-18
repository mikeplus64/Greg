{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Greg.Bot where
import Greg.Types
import Greg.IRC (send)
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Map       as M
import Data.Sequence            as S ((|>), singleton)
import Control.Concurrent
import Control.Monad    (when)

-- | Send a message to the bot's channel.
message :: Bot -> T.Text -> IO ()
message !bot !str = T.hPutStrLn (socket bot) $ "PRIVMSG " `T.append` channel (config bot) `T.append` " :" `T.append` str

parseMessage :: T.Text -> Bot -> Maybe Message
parseMessage !mg !bot = case T.breakOn expected mg of
    ("",  _) -> Nothing
    (_ , "") -> Nothing
    (!talker, !unparsed) -> 
        let (!nick, !host) = T.breakOn "!" (T.init (T.tail talker))
        in Just $ Msg nick host (T.drop (T.length expected) unparsed)
  where
    expected :: T.Text
    expected = "PRIVMSG " `T.append` channel (config bot) `T.append` " :"

parseCommand :: Message -> [Command] -> Maybe (Command, T.Text)
parseCommand (Msg _ _ !m) !cmds = if fst (T.splitAt 1 m) == "~"
    then uncurry getCmd (T.breakOn " " m)
    else Nothing
  where
    getCmd !cmd !args = case lookupByAlias (T.tail (T.strip cmd)) cmds of
            Just !c  -> Just (c, args)
            Nothing  -> Nothing

addToQuotes :: Bot -> Message -> IO ()
addToQuotes !bot (Msg !sr _ !mg) = modifyMVar_ (quotes bot) $ \qs -> return $ 
    if M.member sr qs
        then M.adjust (|> T.init mg) sr qs
        else M.insert sr (singleton (T.init mg)) qs

addToPermissions :: Bot -> T.Text -> Permission -> IO ()
addToPermissions !bot !dude Normal      = modifyMVar_ (permissions bot) $ \ps -> return $ M.delete dude ps -- delete permit as Normal is the default anyway
addToPermissions !bot !dude !permission = modifyMVar_ (permissions bot) $ return . M.insert dude permission

-- | True if a user has the required level to run a command.
ok :: Bot -> T.Text -> Command -> IO Bool
ok !bot !dude !cmd = {-# SCC "ok" #-} do
    ps <- readMVar (permissions bot)
    case M.lookup dude ps of
        Just p  -> return $ p >= reqp cmd
        Nothing -> return $ reqp cmd == Normal 

lookupByAlias :: T.Text -> [Command] -> Maybe Command
lookupByAlias !as !cs = {-# SCC "lookupByAlias" #-} case [ cms | cms <- cs, alias cms == as ] of
    (!c:_) -> Just c
    _      -> Nothing

-- | Run a command, and send the output to the channel, or
-- yell at the person who called the command for an error.
msgCommand :: Bot -> Message -> Command -> IO ()
msgCommand !bot !mg !cmd = {-# SCC "msgCommand" #-} do
    okay <- ok bot (sender mg) cmd
    when okay $ do
            result <- run cmd mg{msg = T.strip (msg mg)} bot
            case result of
                Right !m   -> mapM_ (message bot) (T.lines m)
                Left  !err -> send bot ("NOTICE " `T.append` sender mg `T.append` " :" `T.append` err)
