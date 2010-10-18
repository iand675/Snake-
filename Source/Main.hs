module Main where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import GLGraphics
import Data.IORef
import World
import Snake
-- The main function is of type IO (), since called by the 'real world'
-- This is the entry point for program execution
main :: IO ()
main = do
	-- Initializes GLUT libraries, discards args using _ wildcard
	(progname, _) <- getArgsAndInitialize
	initialWindowSize $= displaySize
	
	-- Registers display options.
	initialDisplayMode $= [DoubleBuffered]
	window <- createWindow "Snake"
	
	-- IORefs are the way to have functionally contained mutable variables.
	-- technically, they are a little more than that, but they are tightly
	-- managed so there's no risk of null pointers or anything like that
	-- Not really recommended generally, but for external libs, it can
	-- come in handy.
	gameState <- newIORef Start
	snake <- newIORef startSnake 
	snakeDir <- newIORef NONE -- NONE from HeadDir data type
	appleTemp <- apples
	appleRef <- newIORef appleTemp
	-- registers the display callback to local variable display
	displayCallback $= display snake appleRef 
	
	-- registers keyboard and mouse input to the keyboardMouse function
	-- The datatype maybe has two constructors: Just a, or Nothing
	-- Setting the keyboardMouseCallback to Nothing allows it to discard input
	keyboardMouseCallback $= Just (keyInput snakeDir)
	
	idleCallback $= Just (idle snake appleRef gameState snakeDir window)
	-- Sets the viewport and game world to be seen as 2d
	-- Borrowed from Monadius code
	initMatrix
	
	-- Once callbacks to functions are registered, enters the game loop
	mainLoop
	destroyWindow window
