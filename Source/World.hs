module World where
import System.Random
import Control.Monad
import Data.IORef
import Data.List
import QStack
import Graphics.UI.GLUT

-- Safe snake area is between coordinates (1, 1) and (81, 81).
-- (0, _), (82, _), (_, 0), and (_, 82) are death for the snake
gridRange :: (Int, Int)
gridRange = (82, 82)

rangeX :: Int
rangeX = fst gridRange
rangeY :: Int
rangeY = snd gridRange

-- Tests a coordinate tuple to see if it is a wall space or not
wall :: (Int, Int) -> Bool
wall (0, _) = True
wall (x, _) | x == rangeX - 1 = True
wall (_, 0) = True
wall (_, y) | y == rangeY - 1 = True
wall (_, _) = False

numApples :: Int
numApples = 5

apples :: IO [(Int, Int)]
apples = replicateM numApples gen

gen :: IO (Int, Int)
gen = do
	x <- getStdRandom (randomR (1, rangeX - 2))
	y <- getStdRandom (randomR (1, rangeY - 2))
	return (x, y)

updateApples :: IORef [(Int, Int)] -> (Int, Int) -> IO ()
updateApples list oldApple = do
	modifiable <- readIORef list
	newApple <- gen
	writeIORef list (newApple : snd (partition (== oldApple) modifiable))
	
intersect :: QStack (Int, Int) -> [(Int, Int)] -> Maybe SpecialEvent
intersect q appleList = (\x -> 
	if x `elem` appleList 
		then Just Apple 
		else
			if (wall x)
				then Just Wall
				else 
					if x `elem` (qStackToList $ snd $ deq q)
						then Just Wall
						else Nothing) (fst $ peek q)

data SpecialEvent = Wall
				  | Apple
				deriving Eq

data GameState = Start
			   | Playing
			   | Dead
			deriving Eq
			