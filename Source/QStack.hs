{-
	QStack v0.1 2008-09-07
	Ian Duncan
	
	QStacks are based off of Eric Kidd's queue API,
	which was in turn based upon Chris Okasaki's book, 
	Purely Functional Data Structures.
	
	QStacks essentially are a list structure which allows
	data to be consed or dropped from both the front and back
	in O(1) amortized time.
-}

module QStack where
import Control.Monad

-- Add values to back, utilize the ones at the front
-- The first List is the front, the second list is the back in reverse order
-- Show and Eq allow the QStack to be printed and tested for equality, respectively
data QStack a = QStack [a] [a]
	deriving (Show, Eq)

-- Declares an instance for the polymorphic function fmap that allows a
-- single argument function to be mapped across a whole data structure
instance Functor QStack where
	fmap f (QStack xs ys) = QStack (map f xs) (map f ys)

-- Returns an empty QStacl
newQStack :: QStack a
newQStack = QStack [] []

-- Tests for emptiness using pattern matching.
-- [] means an empty list, _ is a wildcard
empty :: QStack a -> Bool
empty (QStack [] []) = True
empty _				= False

-- Enqueues a value to the end of the QStack
enq :: QStack a -> a -> QStack a
enq (QStack xs ys) y = QStack xs (y:ys)

-- Deques a value from the front of the QStack,
-- Returning an error if the structure is empty
deq :: QStack a -> (a, QStack a)
deq (QStack [] []) = error "Can't deq from an empty QStack"
deq (QStack (x:xs) ys) = (x, QStack xs ys)
deq (QStack [] ys) = deq (QStack (reverse ys) [])

-- Returns the front value of the QStack without
-- removing it
peek :: QStack a -> (a, QStack a)
peek (QStack [] []) = error "Can't peek from an empty QStack"
peek (QStack [] ys) = let (x:xs) = reverse ys in
						(x, QStack (x:xs) [])
peek (QStack xs ys) = (head xs, QStack xs ys)

-- Pushes a value onto the front of the QStack
push :: QStack a -> a -> QStack a
push (QStack xs []) x = QStack [x] (reverse xs)
push (QStack xs ys) x = QStack (x:xs) ys

-- init for QStacks. Returns everything but the last element
qInit :: QStack a -> QStack a
qInit (QStack [] []) = error "Can't qInit from an empty QStack" 
qInit (QStack xs []) = QStack (init xs) []
qInit (QStack xs (y:ys)) = QStack xs ys

qLast :: QStack a -> a
qLast (QStack [] []) = error "Can't qLast from an empty QStack"
qLast (QStack xs []) = head $ reverse xs
qLast (QStack _ (y:_)) = y

-- Converts a list to a QStack
listToQStack :: [a] -> QStack a
listToQStack = flip QStack []

-- Converts a QStack to a list
qStackToList :: QStack a -> [a]
qStackToList (QStack xs ys) = xs ++ (reverse ys) 
