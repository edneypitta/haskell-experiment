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
                    input <- readline "Command: "
                    getInputs (n - 1) (acc ++ [input])

ms2i :: Maybe String -> Integer
ms2i Nothing  = 0
ms2i (Just s) = read s

parsePosition :: Maybe String -> Maybe (Integer, Integer)
parsePosition Nothing  = Nothing
parsePosition (Just s) = Just (read fst, read snd)
                         where fst:snd:[] = splitOn " " s

parseDirection :: String -> Maybe Direction
parseDirection ('N':[]) = Just North
parseDirection ('E':[]) = Just East
parseDirection ('S':[]) = Just South
parseDirection ('W':[]) = Just West
parseDirection _        = Nothing

parseCommand :: String -> Maybe Command
parseCommand s = case direction of
                   Nothing -> Nothing
                   Just d  -> Just Command { direction = d, steps = steps }
                 where fst:snd:[] = splitOn " " s
                       direction  = parseDirection fst
                       steps      = read snd 

parseCommands :: [Maybe String] -> [Maybe Command] -> Maybe [Command]
parseCommands [] acc     = sequence acc
parseCommands (c:cs) acc = case c of
                            Nothing -> parseCommands cs acc
                            Just s  -> parseCommands cs (acc ++ [parseCommand s])

clean :: Robot -> Integer
clean (Robot p c) = 3

main :: IO ()
main = do
          maybeNumberOfCommands <- readline "Number of commands: "
          maybePosition         <- readline "Initial position: "
          maybeCommands         <- getInputs (ms2i maybeNumberOfCommands) []

          let position = parsePosition maybePosition
          let commands = parseCommands maybeCommands []

          case (position, commands) of
            (Nothing, Nothing)             -> main
            (_, Nothing)                   -> main
            (Nothing, _)                   -> main
            (Just position, Just commands) -> do
                                                let robot = Robot { position = position, commands = commands }
                                                let cleaned = clean robot
                                                putStrLn $ "Cleaned: " ++ show cleaned
                                                main