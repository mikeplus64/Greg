module Greg where

import Greg.Simple
import Greg.IRC     (connect)
import Greg.Types   (Command, Message, Bot (..))

import Data.Text (Text)
import Control.Concurrent
import Control.Monad

-- | Simplest way to get a bot up and running.
-- This function logs to stdout with 'Greg.Simple.monitor', and makes a small IRC client that you can type into
-- to send messages to the channel, using 'Greg.Simple.client'
greg :: String    -- ^ Path to config
     -> [Command] -- ^ available commands
     -> (Message -> Bot -> IO (Maybe Text)) -- ^ Message handler (for messages other than commands)
     -> IO ()

greg path coms msgh = do
    bot <- do
        tmpBot <- createBot path
        connect tmpBot { commands = coms, msgHandler = msgh }

    forkIO  (client  bot)
    forever (monitor bot)
