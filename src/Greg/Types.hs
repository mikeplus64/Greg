module Greg.Types where

import Data.Text            (Text)
import Network              (HostName, PortID (..))
import Control.Concurrent   (MVar)
import Data.Map             (Map)
import Data.Sequence        (Seq)
import System.IO            (Handle)

data Message = Msg {
    sender  :: Text, -- ^ the sender's nick
    shost   :: Text, -- ^ the sender's user@hostmask
    msg     :: Text
}

data Command = Com {
    alias :: Text,
    desc  :: Text,
    run   :: Message -> Bot -> IO (Either Text Text),
    reqp  :: Permission
}

data BotConfig = BC {
    nickname    :: Text,
    username    :: Text,
    realname    :: Text,
    channel     :: Text,
    network     :: (HostName, PortID),
    commandFile :: String,
    quoteFile   :: String,
    permFile    :: String
}

data BotConfigRep =
      Nickname
    | Username
    | Realname
    | Channel
    | Network
    | NetworkPort
    | CommandFile
    | QuoteFile
    | PermFile
  deriving (Show, Read)

-- | Permissions for commands, note that the permission required for a command, even when a user's level is 'Peasant', is entirely
-- up to the command.
data Permission = 
      Peasant
    | Normal
    | Mod
    | Admin
  deriving (Read, Show, Enum, Ord, Eq)

data Bot = Bot {
    quotes      :: MVar (Map Text (Seq Text)),
    permissions :: MVar (Map Text Permission),
    commands    :: [Command],
    msgHandler  :: Message -> Bot -> IO (Maybe Text),
    socket      :: Handle,
    config      :: BotConfig
}

