{-# LANGUAGE TypeOperators #-}
module Greg.Parse (configure) where
import Control.Monad.State
import Greg.Types
import Data.Text (Text, unpack)
import Control.Applicative ((<$>))
import Network

data a := b = a := b
  deriving (Show)

instance (Read a, Read b) => Read (a := b) where
    readsPrec _ x = 
        let w = words x
        in case w of
            (var:args) | last var == ':' -> [(read (init var) := read (unwords args), "")]
            _                            -> []

-- | Read a file, parse it, and build a 'BotConfig' out of it.
configure :: String -> IO BotConfig
configure path = buildConfig <$> mkConfigRep <$> lines <$> readFile path

mkConfigRep :: (Read a, Read b) => [String] -> [a := b]
mkConfigRep readme = evalState (go (zip [1 :: Int ..] readme)) []
  where
    go []            = get
    go (statement:r) = do
        parsed <- get
        case reads (snd statement) :: (Read a, Read b) => [(a := b, String)] of
            [(s, "")] -> do
                put (parsed ++ [s])
                go r
            []        -> go r -- empty line
            _         -> fail $ "invalid configuration at line " ++ show (fst statement) ++ ": " ++ snd statement

buildConfig :: [BotConfigRep := Text] -> BotConfig
buildConfig conf = go conf BC {}
  where
    go []     bot = bot
    go (l:ls) bot = case l of
            Nickname        := b -> go ls bot { nickname    = b }
            Username        := b -> go ls bot { username    = b }
            Realname        := b -> go ls bot { realname    = b }
            Channel         := b -> go ls bot { channel     = b }
            Network         := b -> go ls bot { network     = (unpack b, PortNumber 6667) } -- hack !
            NetworkPort     := b -> go ls bot { network     = (fst (network bot), PortNumber (fromIntegral (read (unpack b) :: Int))) }
            CommandFile     := b -> go ls bot { commandFile = unpack b }
            QuoteFile       := b -> go ls bot { quoteFile   = unpack b }
            PermFile        := b -> go ls bot { permFile    = unpack b }

