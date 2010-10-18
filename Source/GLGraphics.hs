module GLGraphics where
import Control.Concurrent
import Graphics.Rendering.OpenGL hiding (R) -- R conflicts with HeadDir constructor
import Graphics.Rendering.OpenGL.GL.BasicTypes
import Graphics.UI.GLUT hiding (R)
import Data.IORef
import QStack
import Snake
import World


displaySize = Size 640 640
dispSize = (640, 640)

blockSize = blockDimensions dispSize

handy = coordToVert2 blockSize

coordToVert2 :: Integral a => (Int, Int) -> Vertex2 a
coordToVert2 (x, y) = Vertex2 (fromIntegral x) (fromIntegral y)

coordToVert3 :: Integral a => (Int, Int) -> Vertex3 a
coordToVert3 (x, y) = Vertex3 (fromIntegral x) (fromIntegral y) 0

edgeList =  [(x,y) | x <- [0..(fst gridRange - 1)], y <- [0,(snd gridRange - 1)]] ++
			[(x,y) | x <- [0,(fst gridRange - 1)], y <- [0..(snd gridRange - 1)]]

testDraw = map (mult blockSize) edgeList
scaling = map (coordToVert2 . mult blockSize)


borderCoords :: [(Vertex2 GLint, Vertex2 GLint)]
borderCoords = zip (map coordToVert2 testDraw) (map coordToVert2 (map(plus blockSize) testDraw))

appleCoords :: [(Int, Int)] -> [(Vertex2 GLint, Vertex2 GLint)]
appleCoords = ap (zip . map (coordToVert2 . mult blockSize)) (map (coordToVert2 . plus blockSize . mult blockSize))

test = (putStrLn "Hi " ++) . (++ "!")
  

-- genCoords :: [(GLint, GLint)] -> [(Vertex2 GLint, Vertex2 GLint)]
genCoords = liftM2 zip scaling (map (coordToVert2 . plus blockSize))


(a, b) `mult` (c, d) = (a*c, b*d)
(a, b) `plus` (c, d) = (a+c, b+d)

-- Updates the direction the snake is moving, but doesn't actually move the snake.
-- Otherwise, by pressing arrow keys rapidly the snake could be moved faster than
-- the game updates, and there would be some skipping and discontinuity
keyInput :: IORef HeadDir -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyInput dir (SpecialKey a) Down _ _ = do 
	case a of
		KeyUp -> update U
		KeyDown -> update D
		KeyLeft -> update L
		KeyRight -> update R
		where
			update x = do
				d <- readIORef dir
				if x `opposite` d then return() else writeIORef dir x
			opposite U D = True
			opposite D U = True
			opposite L R = True
			opposite R L = True
			opposite _ _ = False
				
-- catches anything that doesn't match the description and just returns a dummy IO action
keyInput _ _ _ _ _ = return ()

-- For some reason, the display function is not having luck snake-drawing...
-- FIXME
display :: IORef (Snake (Int, Int)) -> IORef [(Int, Int)] -> IO ()
display snake apples = do 
		appleList <- readIORef apples
		s <- readIORef snake
		clear [ColorBuffer]
		drawBorder
		drawApples appleList
		drawSnake s
		swapBuffers
		flush

idle snake apples game dir window = do
	state <- readIORef game
	if state == Dead 
		then return ()
		else do
			direction <- readIORef dir
			s <- readIORef snake
			modifyIORef snake (`move` direction)
			updates snake dir apples game
		--  This is where the graphic code goes
			postRedisplay (Just window)
	
drawBorder = preservingMatrix $ do
		color (Color3 0.0 0.0 (1.0 :: Double))
		mapM_ (uncurry rect) borderCoords
	
drawApples list = preservingMatrix $ do
		color (Color3 1.0 0.0 (0.0 :: Double))
		mapM_ (uncurry rect) (appleCoords list)

-- FIXME: don't be lazy and use the appleCoords function to draw the snake	
drawSnake q = preservingMatrix $ do
		color (Color3 0.0 1.0 (0.0 :: Double))
		mapM_ (uncurry rect) (appleCoords $ qStackToList q)

updates :: IORef (Snake (Int, Int)) -> IORef HeadDir -> IORef [(Int, Int)] -> IORef GameState -> IO ()
updates s d a state = do
	snake <- readIORef s
	appleList <- readIORef a
	direction <- readIORef d
	let
		updater :: SpecialEvent -> IO ()
		updater Wall = writeIORef state Dead
		updater Apple = do
			oldTail <- return (qLast snake)
			threadDelay 2000
			modifyIORef s (`move` direction)
			postRedisplay (Nothing)
			modifyIORef s (\x -> enq x oldTail)
			updateApples a (fst $ peek snake)
			
		maybeIO :: Maybe SpecialEvent -> IO ()
		maybeIO x = maybe return updater
		
	maybeIO (intersect snake appleList)
	threadDelay 4000

-- Borrowed from Monadius code
-- Used for 2d drawing in OpenGl	
initMatrix :: IO ()
initMatrix = do
	viewport $= (Position 0 0, displaySize)
	matrixMode $= Projection
	loadIdentity
	perspective 90.0 1 320 1400
	lookAt (Vertex3 320 320 (320 :: Double)) (Vertex3 320 320 (0 :: Double)) (Vector3 0 1 (0 :: Double))

-- Returns the size of the blocks used in the game
-- TODO: figure out how to coerce this into a more natural datatype if it's useful
blockDimensions (x, y) = (x `div` (fst gridRange), y `div` (snd gridRange))
(++ "\nHello")
\x y -> x `div` y