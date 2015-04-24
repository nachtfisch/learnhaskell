{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage x = parseWords . words $ x

parseWords :: [String] -> LogMessage
parseWords ("I":t:r) = LogMessage (Info) (read t :: Int) (unwords r)
parseWords ("W":t:r) = LogMessage (Warning) (read t :: Int) (unwords r)
parseWords ("E":c:t:r) = LogMessage (Error (read c :: Int)) (read t :: Int) (unwords r)
parseWords _ = Unknown "unknown" 

parse :: String -> [LogMessage]
parse x  = map (parseMessage) . lines $ x
