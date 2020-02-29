module Main where

import Robot
import Data.List.Split
import Data.Set (empty)
import System.Console.Readline
import Control.Monad.State

getInputs :: Int -> IO [Maybe String]
getInputs n = sequence $ map (\_ -> readline "Command: ") [1..n]

ms2i :: Maybe String -> Int
ms2i Nothing  = 0
ms2i (Just s) = read s

parsePosition :: Maybe String -> Maybe Position
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
parseCommand s = maybeDirection >>= (\d -> Just Command { direction = d, steps = steps })
                 where fst:snd:[]     = splitOn " " s
                       maybeDirection = parseDirection fst
                       steps          = read snd 

parseCommands :: [Maybe String] -> Maybe [Command]
parseCommands cs = sequence $ foldr (\s acc -> (s >>= parseCommand) : acc) [] cs

main :: IO ()
main = do
          putStrLn "Starting cleaning"
          maybeNumberOfCommands <- readline "Number of commands: "
          maybePosition         <- readline "Initial position: "
          maybeCommands         <- getInputs $ ms2i maybeNumberOfCommands

          let position = parsePosition maybePosition
          let commands = parseCommands maybeCommands

          case (position, commands) of
            (Just position, Just commands) -> do
              let robot = Robot { position = position, commands = commands }
              let cleaned = evalState (clean robot) empty
              putStrLn $ "Cleaned: " ++ show cleaned
          main