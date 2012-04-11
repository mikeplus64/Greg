import Greg
import Greg.Commands.Default
import Greg.Bot (addToQuotes)

main :: IO ()
main = greg "config" defaultCommands $ \m cb -> addToQuotes cb m >> return Nothing
