{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- possible improvement: use reads or readMaybe instead of using read which throws an exception

parseMessage :: String -> LogMessage
parseMessage x = parseWords . words $ x

parseWords :: [String] -> LogMessage
parseWords ("I":t:r) = LogMessage (Info) (read t :: Int) (unwords r)
parseWords ("W":t:r) = LogMessage (Warning) (read t :: Int) (unwords r)
parseWords ("E":c:t:r) = LogMessage (Error (read c :: Int)) (read t :: Int) (unwords r)
parseWords _ = Unknown "unknown" 

parse :: String -> [LogMessage]
parse x  = map (parseMessage) . lines $ x

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert a Leaf = Node Leaf a Leaf
insert i@(LogMessage _ t1 _) (Node l cur@(LogMessage _ t2 _) r) 
  | t1 <= t2 = Node (insert i l) (cur) r
  | otherwise = Node l (cur) (insert i r)
insert _ _ = error "insert into a tree containing unknown messages is not supported"

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (h:r) = insert h (build r)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = [] 
inOrder (Node lt mt rt) = (inOrder lt) ++  [mt] ++ (inOrder rt) 
