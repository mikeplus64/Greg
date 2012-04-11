module Greg.Types where

import Data.Text            (Text)
import Network              (HostName, PortID (..))
import Control.Concurrent   (MVar)
import Data.Map             (Map)
import Data.IntMap          (IntMap)
import System.IO            (Handle)

data Message = Msg {
    sender  :: Text,
    shost   :: Text,
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

data Permission = 
      Peasant
    | Normal
    | Mod
    | Admin
  deriving (Enum, Show, Read)

data Bot = Bot {
    quotes      :: MVar (Map Text (IntMap Text)),
    permissions :: MVar (Map Text Permission),
    commands    :: [Command],
    msgHandler  :: Command,
    socket      :: Handle,
    config      :: BotConfig
}
