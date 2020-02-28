module Main where

import Data.List.Split
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

ms2i :: Maybe String -> Integer
ms2i Nothing  = 0
ms2i (Just s) = read s

getPosition :: Maybe String -> Maybe (Integer, Integer)
getPosition Nothing  = Nothing
getPosition (Just s) = Just (read fst, read snd)
                       where fst:snd:[] = splitOn " " s

main :: IO ()
main = do
          maybeNumberOfCommands <- readline ""
          maybePosition         <- readline ""
          maybeCommands         <- getInputs (ms2i maybeNumberOfCommands) []
          
          case maybePosition of
            Nothing     -> return ()
            Just "quit" -> return ()
            Just input  -> do 
                             putStrLn $ "Input was: " ++ input
                             main