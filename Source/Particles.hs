module Particles where
import Data.Reactive
maxParticles = 1000
slowdown = 2
data Particle = {
				life :: Integer,
				color :: Triple Double,
				position :: Triple Double -> Triple Double,
				direction :: Triple Double,
				gravity :: Triple Double
				}
type Triple a = (a, a, a)

createParticles :: Random a => Int -> Integer -> Triple Double -> Double -> Triple Double -> (Triple Double -> Triple Double) -> Triple Double -> RandomState a -> Particle
createParticles amount duration center proximity color dir grav = 
	take amount repeat $ Particle {life = duration, direction = dir, gravity = grav}
	where 
		process :: Random a => Particle -> RandomState a -> Particle
		process part gen= part{}
	
--      State TypeOfInput TypeofOutput
tick :: State Int Int
tick = do n <- get
		  put (n+1)
		  return n
		
randomR (center - prox, center + prox)

type RandomState a = State StdGen a




getRandomR :: Random a => RandomState (a, a)
getRandomR =
	get >>= \gen ->
	let (val, gen') = randomR gen in
	put gen' >> return val

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom



type Color = Triple Int
class Particle a where
	move :: ReactiveB -> 
	color :: ReactiveB -> Color
	
-- Signal a = Time -> a
-- Signal a is a function mapping the appropriate values of time to a value of type t
-- signal s's value at time t is just s(t)

-- Signal has space leaks, so there is only access to signal transformers aka signal functions:
-- SF a b = Signal a -> Signal b
-- transforms 

{-
non point free style:
g :: A -> C
g x = f2 (f1 x)
point free style:
g = f2 . f1

for arrows, an operator to lift ordinary functions to signal functions:
arr :: (a -> b) -> SF a b
a combinator to compose signal functions:
(>>>) :: SF a b -> SF b c -> SF a c
>>> is like a reverse function composition
Now we can write
g' :: SF A C
g' = arr g
g' = arr f1 >>> arr f2

What if we want to write this in a point free style:
h :: A -> (B, C)
h x = (f1 x, f2 x)
We can write that by defining a combinator:
(&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(f1 & f2) x = (f1 x, f2 x)
Now the answer is just:
h = f1 & f2

for arrows, the combinator is:
(&&&) :: SF a b -> SF a c -> SF a (b, c)

Say that we have this:
f1 :: A -> B
f2 :: C -> D
A point free version of:
i :: (A, C) -> (B, D)
i (x, y) = (f1 x, f2 y)
could be written like this:
i = (f1 . fst) & (f2 . snd)
i' = arr i
i' = arr (f1 . fst) &&& arr (f2 . snd)
i' = (arr fst >>> arr f1) &&& (arr snd >>> arr f2)
This is common? So, the following combinator is useful:
(***) :: SF b c -> SF b' c' -> SF (b, b') (c, c')
f *** g = (arr fst >>> f) &&& (arr snd >>> g)
Now i' is:
i' = arr f1 *** arr f2


class Arrow a where
	arr   :: (b -> c) -> a b c
	(>>>) :: a b c -> a c d -> a b d
	first :: a b c -> a (b, d) (c, d)
	

-}