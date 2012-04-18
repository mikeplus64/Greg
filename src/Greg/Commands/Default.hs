{-# LANGUAGE OverloadedStrings #-}
module Greg.Commands.Default (defaultCommands, success, failure) where

import System.Random        (randomRIO)
import System.Process       (createProcess, proc, CreateProcess (std_out), StdStream (..))
import Control.Concurrent   (readMVar, modifyMVar_)

import qualified Data.Text.IO   as T
import qualified Data.Text      as T
import Data.Text.Read (decimal)
import qualified Data.Map       as M
import qualified Data.Sequence  as S

import Greg.Types
import Greg.Bot

defaultCommands :: [Command]
defaultCommands = [echo, help, dementia, quoteview, rq, quote_, visudo, offend, permit, remember, level, fortune_, quotesinfo, quoteinfo]


echo, help, dementia, quoteview, rq, quote_, visudo, offend, permit, remember, level, fortune_, quoteinfo, quotesinfo :: Command
echo = Com {
    alias = "echo",
    desc  = "echo something!",
    reqp  = Mod,
    run   = \m _ -> success $ msg m
}
 
help = Com {
    alias = "help",
    desc  = "get help bro :(",
    reqp  = Normal,
    run   = \m b ->
        case lookupByAlias (msg m) (commands b) of
            Just cmd -> success $ desc cmd
            _        -> failure "command not found"
}

dementia = Com {
    alias = "dementia",
    desc  = "forget all of a person's quotes",
    reqp  = Mod,
    run   = \m b -> do
        modifyMVar_ (quotes b) (return . M.delete (msg m))
        success "OK."
}

quoteinfo = Com {
    alias = "quote-info",
    desc  = "learn about a person in the quote db",
    reqp  = Normal,
    run   = \m b -> do
        qs <- readMVar (quotes b)
        let quoteTotal = T.pack $ show $ case M.lookup (msg m) qs of
                Just iqs -> S.length iqs
                _        -> 0
        success $ sender m `T.append` ": " `T.append` quoteTotal `T.append` " quotes from this person in the db."
}

quotesinfo = Com {
    alias = "quotes-info",
    desc  = "learn about quotes",
    reqp  = Normal,
    run   = \m b -> do
        qs <- readMVar (quotes b)
        let totalDudes  = T.pack $ show $ M.size qs
            totalQuotes = T.pack $ show $ sum $ map (S.length . snd) (M.toList qs)
        success $ sender m `T.append` ": " `T.append` totalDudes `T.append` " people in the db, " `T.append` totalQuotes `T.append` " quotes in total."
}

quoteview = Com {
    alias = "quote-view",
    desc  = "get the nth quote from someone, eg ~quote-view guy 345",
    reqp  = Normal,
    run   = \m b -> do
        qs <- readMVar (quotes b)
        let ws = T.words (msg m)
        if length ws == 2
            then case M.lookup (head ws) qs of
                    Just iqs -> case decimal (ws !! 1) of -- decimal (T.unpack (ws !! 1)) :: [(Int, String)] of
                        Right (i, "") | i <= S.length iqs -> success $ sender m `T.append` ": " `T.append` S.index iqs i
                        _ -> failure "not found"
                    _ -> failure "this person has no quotes"
            else failure "bad arguments"
}

rq = quote_ { alias = "rq" } -- an awesome hack
quote_ = Com {
    alias = "quote",
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
                        let (nick, quoteSeq) = M.elemAt senderIndex qs
                        quoteIndex  <- randomRIO (0, S.length quoteSeq - 1)
                        let quote = quoteSeq `S.index` quoteIndex
                        success $ "<" `T.append` nick `T.append` "> " `T.append` quote
            else do
                qs <- readMVar (quotes b)
                case M.lookup (msg m) qs of
                    
                    Just quoteSeq -> do
                        quoteIndex <- randomRIO (0, S.length quoteSeq - 1)
                        let quote = quoteSeq `S.index` quoteIndex
                        success $ "<" `T.append` msg m `T.append` "> " `T.append` quote
                    
                    _ -> failure "YOU LOSE!"
}


visudo = Com {
    alias = "visudo",
    desc  = "grant any permission",
    reqp  = Admin,
    run   = \(Msg _ _ m) bot -> case T.words m of
        [dude, newPermit] -> case decimal newPermit of
            Right (p, "") | p < 4 -> do 
                addToPermissions bot dude (toEnum p)
                success "OK."
            _ -> failure "sorry dave!"
        _ -> failure "bad arguments dave!"
}

fortune_ = Com {
    alias = "fortune",
    desc  = "sporadically-daily fortunes",
    reqp  = Normal,
    run   = \_ _ -> do
        (_, Just fortuneHandle, _, _) <- createProcess (proc "fortune" ["-s"]) { std_out = CreatePipe }
        fortune <- T.hGetContents fortuneHandle
        success fortune
}

offend = Com {
    alias = "offend",
    desc  = "too many friends? offend someone!",
    reqp  = Normal,
    run   = \(Msg _ _ target) _ -> do
        (_, Just fortuneHandle, _, _) <- createProcess (proc "fortune" ["-os"]) { std_out = CreatePipe }
        fortune <- T.hGetContents fortuneHandle
        success $ (if T.null target then "" else target `T.append` ": ") `T.append` fortune
}

permit = Com {
    alias = "permit",
    desc  = "get a permit",
    reqp  = Mod,
    run   = \(Msg _ _ m) bot -> case T.words m of
        [dude, newPermit] -> case decimal newPermit of
            Right (p, "") | p < 4 -> do
                addToPermissions bot dude (toEnum p)
                success "OK"
            _ -> failure "sorry dave"
        _ -> failure "sorry dave"
}

remember = Com {
    alias = "remember",
    desc  = "remember a quote",
    reqp  = Mod,
    run   = \(Msg _ _ m) bot -> case T.breakOn " " m of
        ("", _) -> failure "can't do that"
        (_, "") -> failure "can't do that"
        (dude, quote) -> do
            addToQuotes bot (Msg dude undefined (T.tail quote))
            success "OK"
}

level = Com {
    alias = "level",
    desc  = "get your level!",
    reqp  = Normal,
    run   = \mg bot -> do
        ps <- readMVar (permissions bot)
        maybe (success "Normal") (success . T.pack . show) (M.lookup (if T.null (msg mg) 
            then sender mg 
            else msg mg) ps)
}

{-# INLINE success #-}
success :: a -> IO (Either b a)
success = return . Right

{-# INLINE failure #-}
failure :: a -> IO (Either a b)
failure = return . Left
