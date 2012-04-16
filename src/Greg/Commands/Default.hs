{-# LANGUAGE OverloadedStrings #-}
module Greg.Commands.Default (defaultCommands, success, failure) where

import System.Random        (randomRIO)
import System.Process       (createProcess, proc, CreateProcess (std_out), StdStream (..))
import Control.Concurrent   (readMVar, modifyMVar_)

import qualified Data.Text.IO as T
import qualified Data.Text    as T
import qualified Data.Map     as M
import qualified Data.IntMap  as I

import Greg.Types
import Greg.Bot
import Data.Char (toUpper)

defaultCommands :: [Command]
defaultCommands = [   
        Com {
            alias = "echo",
            desc  = "echo something!",
            reqp  = Mod,
            run   = \m _ -> success $ msg m
        },
        Com {
            alias = "help",
            desc  = "get help bro :(",
            reqp  = Normal,
            run   = \m b ->
                case lookupByAlias (msg m) (commands b) of
                    Just cmd -> success $ desc cmd
                    _        -> failure "command not found"
        },
        Com {
            alias = "dementia",
            desc  = "forget all of a person's quotes",
            reqp  = Mod,
            run   = \m b -> do
                modifyMVar_ (quotes b) (return . M.delete (msg m))
                success "OK."
        },
        Com {
            alias = "quote-view",
            desc  = "get the nth quote from someone, eg ~quote-view guy 345",
            reqp  = Normal,
            run   = \m b -> do
                qs <- readMVar (quotes b)
                let ws = T.words (msg m)
                if length ws == 2
                    then case M.lookup (head ws) qs of
                            Just iqs -> case reads (T.unpack (ws !! 1)) :: [(Int, String)] of
                                [(i, "")] -> case I.lookup i iqs of
                                    Just q -> success $ sender m `T.append` q
                                    _      -> failure "quote at this index not found"
                                _         -> failure "cannot parse second argument"
                            _        -> failure "this person has no quotes"
                    else failure "bad arguments"
        },
        (defaultCommands !! 5) { alias = "quote" }, -- an awesome hack
        Com {
            alias = "rq",
            desc  = "get a quote!",
            reqp  = Normal,
            run   = \m b ->
                if T.null (msg m) 
                    then do
                        qs <- readMVar (quotes b)
                        if M.null qs
                            then failure "No quotes available!"
                            else do
                                senderIndex <- randomRIO (0, M.size qs - 1)
                                let (nick, quoteMap) = M.elemAt senderIndex qs
                                quoteIndex  <- randomRIO (0, I.size quoteMap - 1)
                                let quote = quoteMap I.! quoteIndex
                                success $ "<" `T.append` nick `T.append` "> " `T.append` quote
                    else do
                        qs <- readMVar (quotes b)
                        case M.lookup (msg m) qs of
                            
                            Just quoteMap -> do
                                quoteIndex <- randomRIO (0, I.size quoteMap - 1)
                                let quote = quoteMap I.! quoteIndex
                                success $ "<" `T.append` msg m `T.append` "> " `T.append` quote
                            
                            _ -> failure "YOU LOSE!"
        },
        Com {
            alias = "visudo",
            desc  = "grant Admin",
            reqp  = Admin,
            run   = \(Msg _ _ m) bot -> case T.words m of
                [dude, newPermit] -> case reads ((\t -> let c = toUpper $ head t in c : tail t) $ T.unpack newPermit) :: [(Permission, String)] of
                    [(p, "")] -> do
                        addToPermissions bot dude p
                        success "OK."
                    _ -> failure "sorry dave"
                _ -> failure "sorry dave"
        },
        Com {
            alias = "fortune",
            desc  = "sporadically-daily fortunes",
            reqp  = Normal,
            run   = \_ _ -> do
                (_, Just fortuneHandle, _, _) <- createProcess (proc "fortune" ["-s"]) { std_out = CreatePipe }
                fortune <- T.hGetContents fortuneHandle
                success fortune
        },
        Com {
            alias = "offend",
            desc  = "too many friends? offend someone!",
            reqp  = Normal,
            run   = \(Msg _ _ target) _ -> do
                (_, Just fortuneHandle, _, _) <- createProcess (proc "fortune" ["-os"]) { std_out = CreatePipe }
                fortune <- T.hGetContents fortuneHandle
                success $ (if T.null target then "" else target `T.append` ": ") `T.append` fortune
        },
        Com {
            alias = "permit",
            desc  = "get a permit",
            reqp  = Mod,
            run   = \(Msg _ _ m) bot -> case T.words m of
                [dude, newPermit] -> case reads (T.unpack newPermit) :: [(Permission, String)] of
                    [(Admin,  "")] -> failure "sombrero"
                    [(Normal, "")] -> do
                        modifyMVar_ (permissions bot) $ \ps -> return $ M.delete dude ps -- delete permit as Normal is the default anyway
                        success "OK."
                    [(p, "")] -> do
                        addToPermissions bot dude p
                        success "OK."
                    _ -> failure "sorry dave"
                _ -> failure "sorry dave"
        },
        Com {
            alias = "remember",
            desc  = "remember a quote",
            reqp  = Mod,
            run   = \(Msg _ _ m) bot -> case T.breakOn " " m of
                ("", _) -> failure "can't do that"
                (_, "") -> failure "can't do that"
                (dude, quote) -> do
                    addToQuotes bot (Msg dude undefined (T.tail quote))
                    success "OK"
        },
        Com {
            alias = "level",
            desc  = "get your level!",
            reqp  = Normal,
            run   = \mg bot -> do
                ps <- readMVar (permissions bot)
                maybe (success "Normal") (success . T.pack . show) (M.lookup (if T.null (msg mg) 
                    then sender mg 
                    else msg mg) ps)
        }
    ]


success :: a -> IO (Either b a)
success = return . Right

failure :: a -> IO (Either a b)
failure = return . Left
