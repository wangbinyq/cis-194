module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = case words str of
   ("E" : err : time :rest) -> LogMessage (Error (read err :: Int)) (read time :: TimeStamp) (unwords rest)
   ("I" : time :rest) -> LogMessage Info (read time :: TimeStamp) (unwords rest)
   ("W" : time :rest) -> LogMessage Warning (read time :: TimeStamp) (unwords rest)
   _ -> Unknown str

parse :: String -> [LogMessage]
parse str = inOrder (build [parseMessage(line) | line <- lines str])

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ nt _) (Node lt mt@(LogMessage _ t _) rt) = 
  case nt < t of
    True -> Node (insert msg lt) mt rt
    False -> Node lt mt (insert msg rt)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (lm : lms) = insert lm (build lms)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt lm rt) = (inOrder lt) ++ [lm] ++ (inOrder rt)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error n) _ s): lms) = case n >= 50 of
                            True -> s : (whatWentWrong lms)
                            _ -> (whatWentWrong lms)
whatWentWrong (lm: lms) = (whatWentWrong lms)

main file = testWhatWentWrong parse whatWentWrong file