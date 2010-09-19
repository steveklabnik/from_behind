import UI.HSCurses.Curses
import System.IO
import Text.Printf

type Health = Int
type Name = String

data Player = Player Name Health

data Cell = Wall
					| Empty
					| Goal

instance Show Cell where 
	show Wall = "#"
	show Empty = "."
	show Goal = "<"


data World = MakeWorld [[Cell]] Int Int

initWorld :: IO World
initWorld = return $ MakeWorld [[Empty, Wall], [Empty, Goal]] 0 0

initPlayer :: IO Player
initPlayer = return $ Player "Someone" 10

act :: World -> Key -> IO World
act (MakeWorld board x y) i = case i of
	KeyChar 'h' -> return $ MakeWorld board (lowerBound (x - 1)) y 
	KeyChar 'j' -> return $ MakeWorld board x (upperBound (y + 1))
	KeyChar 'k' -> return $ MakeWorld board x (lowerBound (y - 1))
	KeyChar 'l' -> return $ MakeWorld board (upperBound (x + 1)) y 
	otherwise -> return $ MakeWorld board x y

lowerBound :: Int -> Int
lowerBound n = max n 0

upperBound :: Int -> Int
upperBound n = min n 1

drawWorld :: World -> IO ()
drawWorld (MakeWorld board x y) = do
	wclear stdScr
	move 0 0
	board' <- return $ foldr (++) "" $ foldr (\a b -> (map show a) ++ ["\n"] ++ b) [] board
	wAddStr stdScr board'
	wMove stdScr y x
	wAddStr stdScr "@"
	move 15 0
	wAddStr stdScr $ "x: " ++ show x ++ " y: " ++ show y
	refresh

hasWon :: World -> Bool
hasWon w = True

gameLoop :: Int -> World -> Player -> IO World
gameLoop n w p 
	| hasWon w == (n == 10) = return w
	| otherwise = do 
		i <- getCh
		w' <- act w i
		drawWorld w'
		gameLoop (n + 1) w' p

main = do
	hSetBuffering stdout NoBuffering
	initCurses
	cBreak True
	echo False
	nl False
	world <- initWorld
	player <- initPlayer
	gameLoop 0 world player
	endWin
	putStr "Goodbye!"

