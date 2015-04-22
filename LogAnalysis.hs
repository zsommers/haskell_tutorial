{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s
  | mt == "E" = LogMessage (Error num1) num2 (remove 3)
  | mt == "W" = LogMessage Warning num1 (remove 2)
  | mt == "I" = LogMessage Info num1 (remove 2)
  | otherwise = Unknown s
  where ws = words s
        mt = head ws
        num1 = read (ws !! 1)
        num2 = read (ws !! 2)
        remove n = unwords $ drop n ws

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ newTS _) (Node l v@(LogMessage _ oldTS _) r)
  | newTS < oldTS = Node (insert lm l) v r
  | otherwise     = Node l v (insert lm r)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l v r) = (inOrder l) ++ (v:(inOrder r))

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ m) -> m) . inOrder . build . filter f
                where f (LogMessage (Error sev) _ _)
                        | sev >= 50 = True
                        | otherwise = False
                      f _ = False

getWarns :: [LogMessage] -> [String]
getWarns = map (\(LogMessage _ _ m) -> m) . inOrder . build . filter f
           where f (LogMessage Warning _ m)
                   | elem ':' m             = False
                   | otherwise              = True
                 f _                        = False
