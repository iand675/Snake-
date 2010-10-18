module Snake where
import QStack
import World


-- Declares a type synonym, so writing Snake as a type is the
-- same thing as writing QStack. It's mostly for legibility
type Snake a = QStack a

-- U, D, L, R, and None are data constructors for the different directions
data HeadDir = U | D | L | R | NONE
	deriving (Eq)

-- Pattern matches the HeadDir data type and uses the different constructors to
-- update the snake's location
move :: Snake (Int, Int) -> HeadDir -> Snake (Int, Int)
move q dir = 
	let 
		-- fst takes the first value in a two-valued tuple
		snakeHead = fst $ deq q
		-- clever syntactic sugar that makes x the result of fst snakeHead
		-- and y the result of snd snakeHead
		(x,y) = (fst snakeHead, snd snakeHead)
		update = qInit . push q

	in
		case dir of
			U -> update (x, y + 1)
			D -> update (x, y - 1)
			L -> update (x - 1, y)
			R -> update (x + 1, y)
			-- Leaves the snake un-updated in the beginning until a direction is pressed.
			NONE -> q

-- Attempts to give the starting snake coordinates in the center of the screen
-- I can't figure out how to replace Snake's constructor with something other than
-- QStack
startSnake :: Snake (Int, Int)
startSnake = QStack [(fst gridRange `div` 2, snd gridRange `div` 2)] []

