module Main where

import Data.List.Split
import Data.Set (Set, size, fromList, union, empty)
import System.Console.Readline
import Control.Monad.State

type Position = (Int, Int)
type CleaningResult = Int
type CleaningState = Set Position

data Direction = North | East | South | West deriving (Eq, Show, Read)

data Command = Command 
  { direction :: Direction
  , steps     :: Int
  } deriving (Eq, Show, Read)

data Robot = Robot 
  { position :: Position
  , commands :: [Command] 
  } deriving (Eq, Show, Read)

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

walk :: Direction -> Position -> Position
walk North (x, y) = (x, y + 1)
walk East  (x, y) = (x + 1, y)
walk South (x, y) = (x, y - 1)
walk West  (x, y) = (x - 1, y)

execCommand :: Position -> Command -> [Position]
execCommand p (Command d s) = scanl (const . walk d) p $ replicate s p
                              
clean :: Robot -> State CleaningState CleaningResult
clean (Robot p [])     = do 
                           passed <- get
                           return $ size passed 
clean (Robot p (c:cs)) = do
                           passed <- get
                           let walkedThrough = execCommand p c
                           put $ union passed $ fromList walkedThrough
                           clean Robot { position = last walkedThrough, commands = cs }
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