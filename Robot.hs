module Robot where

import Control.Monad.State
import Data.Set (Set, size, fromList, union)

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

walk :: Direction -> Position -> Position
walk North (x, y) = (x, y + 1)
walk East  (x, y) = (x + 1, y)
walk South (x, y) = (x, y - 1)
walk West  (x, y) = (x - 1, y)

execCommand :: Position -> Command -> [Position]
execCommand p (Command d s) = scanl (const . walk d) p $ replicate s p
                              
clean :: Robot -> State CleaningState CleaningResult
clean     (Robot _ []) = do 
                           passed <- get
                           return $ size passed 
clean (Robot p (c:cs)) = do
                           passed <- get
                           let walkedThrough = execCommand p c
                           put $ union passed $ fromList walkedThrough
                           clean Robot { position = last walkedThrough, commands = cs }