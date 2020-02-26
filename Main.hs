module Main where

  import Data.HashSet
  import Text.Read
  import Data.Maybe
  import System.Console.Haskeline
  import Control.Monad
  
  data Direction = North | East | South | West deriving (Eq, Show, Read)

  data Command = Command 
    { direction :: Direction
    , steps :: Integer 
    } deriving (Eq, Show, Read)

  data Robot = Robot 
    { position :: (Integer, Integer)
    , commands :: [Command] 
    } deriving (Eq, Show, Read)

  main :: IO ()
  main = runInputT defaultSettings loop
    where
      loop :: InputT IO ()
      loop = do
        maybeNumberOfCommands <- getInputLine ""
        position <- getInputLine ""

        let numberOfCommands = fromMaybe 0 $ maybeNumberOfCommands >>= readMaybe
        --let h = Prelude.map Nothing [0..numberOfCommands] :: Maybe String
        let q = foldM(\acc _ -> do
                                  input <- getInputLine ""
                                  outputStrLn $ input
                                  return acc:input) [] replicate numberOfCommands Nothing

        
        case position of
          Nothing -> return ()
          Just "quit" -> return ()
          Just input -> do 
                          outputStrLn $ "Input was: " ++ input
                          loop