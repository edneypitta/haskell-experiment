module Main where

import Data.HashSet
import Text.Read
import Data.Maybe
import System.Console.Readline
import Control.Monad
import Control.Monad.IO.Class

data Direction = North | East | South | West deriving (Eq, Show, Read)

data Command = Command 
  { direction :: Direction
  , steps     :: Integer 
  } deriving (Eq, Show, Read)

data Robot = Robot 
  { position :: (Integer, Integer)
  , commands :: [Command] 
  } deriving (Eq, Show, Read)

getInputs :: Integer -> [Maybe String] -> IO [Maybe String]
getInputs 0 acc = return acc
getInputs n acc = do
                    input <- readline ""
                    getInputs (n - 1) (acc ++ [input])

main :: IO ()
main = do
          maybeNumberOfCommands <- readline ""
          position <- readline ""

          let numberOfCommands = fromMaybe 0 $ maybeNumberOfCommands >>= readMaybe
          commands <- getInputs numberOfCommands []
          
          case position of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input -> do 
                            putStrLn $ "Input was: " ++ input
                            main