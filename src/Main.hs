import Greg.Simple
import Greg.Commands.Default
import Greg.Bot (addToQuotes)
import Greg.IRC (connect)
import Greg.Types

import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
    bot <- do
        b <- createBot "/home/opuk/.greg/config"
        let b' = b { 
            commands   = defaultCommands,
            msgHandler = \m cb -> do
                addToQuotes cb m 
                return Nothing
            }
        connect b'
    
    forkIO  (client  bot)
    forever (monitor bot)
