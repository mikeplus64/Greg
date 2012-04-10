{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Bot where
import Types
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Map    as M
import qualified Data.IntMap as I
import Control.Concurrent
import Control.Arrow
import System.Random    (randomRIO)
import System.Process   (createProcess, proc, CreateProcess (std_out), StdStream (..))

message :: Bot -> T.Text -> IO ()
message !bot !str = T.hPutStrLn (socket bot) $ "PRIVMSG " `T.append` channel (config bot) `T.append` " :" `T.append` str

parseMessage :: T.Text -> Bot -> Maybe Message
parseMessage !mg !bot = case T.breakOn expect mg of
    ("",  _) -> Nothing
    (_ , "") -> Nothing
    (talker, unparsed) -> case parseNick talker of
        Just (nick, _) -> Just (Msg nick (parseMsg unparsed))
        _ -> Nothing
  where
    expect = "PRIVMSG " `T.append` channel (config bot) `T.append` " :"

    parseMsg = T.drop (T.length expect)

    parseNick !n = 
        let nickAtHost = T.init (T.tail n) -- remove :, strip end space
        in case T.breakOn "!~" nickAtHost of
            ("",  _)    -> Nothing
            (_ , "")    -> Nothing
            nh          -> Just nh

parseCommand :: Message -> [Command] -> Maybe (Command, T.Text)
parseCommand (Msg _ !m) !cmds = if fst (T.splitAt 1 m) == "~"
    then uncurry getCmd (T.breakOn " " m)
    else Nothing
  where
    getCmd cmd args = case lookupByAlias ((T.tail . T.strip) cmd) cmds of
            Just c  -> Just (c, args)
            Nothing -> Nothing

addToQuotes :: Bot -> Message -> IO ()
addToQuotes !bot (Msg !sr !mg) = modifyMVar_ (quotes bot) $ \qs -> return $ if M.member sr qs
    then M.adjust (\is -> I.insert (fst (I.findMax is) + 1) mg is) sr qs
    else M.insert sr (I.singleton 0 mg) qs

addToPermissions :: Bot -> T.Text -> Permission -> IO ()
addToPermissions !bot !dude !permission = modifyMVar_ (permissions bot) $ \ps -> return $ M.insert dude permission ps

lookupByAlias :: T.Text -> [Command] -> Maybe Command
lookupByAlias as cs = lookup as $ map (alias &&& id) cs

msgCommand :: (Command, T.Text, T.Text) -> Bot -> IO ()
msgCommand (!cmd, !s, !args) !bot = do
    result <- run cmd (Msg s (T.strip args)) bot
    case result of
        Right m  -> message bot m
        Left err -> message bot err

defaultCommands :: [Command]
defaultCommands = [   
        Com {
            alias = "echo",
            desc  = "echo something!",
            reqp  = Mod,
            run   = \m _ -> return (Right (msg m))
        },
        Com {
            alias = "help",
            desc  = "get help bro :(",
            reqp  = Normal,
            run   = \m b -> return $
                case lookupByAlias (msg m) (commands (config b)) of
                    Just cmd -> Right $ desc cmd
                    _        -> Left  "command not found"
        },
        Com {
            alias = "quote",
            desc  = "get a quote!",
            reqp  = Normal,
            run   = \m b ->
                if T.null (msg m) 
                    then do
                        qs <- readMVar (quotes b)
                        if M.null qs
                            then return (Left "No quotes available!")
                            else do
                                senderIndex <- randomRIO (0, M.size qs - 1)
                                let (nick, quoteMap) = M.elemAt senderIndex qs
                                quoteIndex  <- randomRIO (0, I.size quoteMap - 1)
                                let quote = quoteMap I.! quoteIndex
                                return (Right ("<" `T.append` nick `T.append` "> " `T.append` quote))
                    else do
                        qs <- readMVar (quotes b)
                        case M.lookup (msg m) qs of
                            Just quoteMap -> do
                                quoteIndex <- randomRIO (0, I.size quoteMap - 1)
                                let quote = quoteMap I.! quoteIndex
                                return (Right ("<" `T.append` msg m `T.append` "> " `T.append` quote))
                            Nothing       -> return (Left "YOU LOSE!")
        },
        Com {
            alias = "fortune",
            desc  = "sporadically-daily fortunes",
            reqp  = Normal,
            run   = \_ _ -> do
                (_, Just fortuneHandle, _, _) <- createProcess (proc "fortune" ["-s"]) { std_out = CreatePipe }
                fortune <- T.hGetContents fortuneHandle
                return (Right fortune)
        }
    ]
