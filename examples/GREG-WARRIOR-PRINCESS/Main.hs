{-# LANGUAGE OverloadedStrings #-}
import Greg
import Greg.Types
import Greg.Commands.Default
import Greg.Bot                      (addToQuotes)

import System.Exit                   (ExitCode (..))

import Control.Arrow                 (second)

import Data.Text                as T (breakOn, drop, unpack, append, null, lines)
import qualified Data.Text.IO   as T (hGetContents, hPutStr)

import System.Process                (waitForProcess, createProcess, proc, CreateProcess (std_out, std_in, std_err), StdStream (..))
import System.IO                     (hClose)

mueval :: Command
mueval = Com {
    alias = "eval",
    reqp  = Mod,
    desc  = "Evaluate a Haskell expression.",
    run   = \m _ -> do
        let (expression, extension) = second (T.drop 2) (T.breakOn "-X" (msg m))

        (_, Just muevalOut, Just muevalErr, h) <- createProcess (proc "mueval" (["-e", unpack expression] ++ if T.null extension then [] else ["-X", unpack extension])) { 
            std_out = CreatePipe, 
            std_err = CreatePipe 
        }
        
        output <- do
            stdout <- T.hGetContents muevalOut
            stderr <- T.hGetContents muevalErr
            return (stdout `append` stderr)

        exitCode <- waitForProcess h
        case exitCode of
            ExitSuccess   -> success (sender m `append` ": " `append` output)
            ExitFailure _ -> if length (T.lines output) < 5
                then success (sender m `append` ": " `append` output)
                else do
                    (Just pasteInput, Just pasteOutput, _, _) <- createProcess (proc "curlpaste" ["--stdin"]) {
                        std_in = CreatePipe, 
                        std_out = CreatePipe 
                    }

                    T.hPutStr pasteInput output
                    hClose pasteInput
                    uri <- T.hGetContents pasteOutput
                    success (sender m `append` ": " `append` uri)

}

main :: IO ()
main = greg "config" (mueval:defaultCommands) $ \m cb -> addToQuotes cb m >> return Nothing
