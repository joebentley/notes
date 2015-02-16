import System.Console.ArgParser
import System.IO
import System.Directory


data ArgsSpec = ArgsSpec {
  note :: String,
  num  :: Int
} deriving (Show)

argParser :: ParserSpec ArgsSpec
argParser = ArgsSpec
  `parsedBy` optPos "" "note" `Descr` "The note to be added"
  `andBy` optFlag 10 "num" `Descr` "Number of records to be shown"

filepath :: IO FilePath
filepath = getHomeDirectory >>= \x -> return (x ++ "/.notes")

run :: ArgsSpec -> IO ()
run args
  | null $ note args =
    filepath >>= readFile >>= mapM_ putStrLn . take (num args) . reverse . lines
  | otherwise =
    filepath >>= \x -> appendFile x (note args ++ "\n")


main = withParseResult argParser run
