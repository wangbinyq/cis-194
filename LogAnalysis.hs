module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = case words str of
   ("E" : err : time :rest) -> LogMessage (Error (read err :: Int)) (read time :: TimeStamp) (unwords rest)
   ("I" : time :rest) -> LogMessage Info (read time :: TimeStamp) (unwords rest)
   ("W" : time :rest) -> LogMessage Warning (read time :: TimeStamp) (unwords rest)
   _ -> Unknown str

parse :: String -> [LogMessage]
parse str = [parseMessage(line) | line <- lines str]

main = testParse parse 10 "error.log"