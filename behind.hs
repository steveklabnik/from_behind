import UI.HSCurses.Curses
import System.IO

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
	KeyChar 'j' -> return $ MakeWorld board x (lowerBound (y - 1))
	KeyChar 'k' -> return $ MakeWorld board x (upperBound (y + 1))
	KeyChar 'l' -> return $ MakeWorld board (upperBound (x + 1)) y 
	otherwise -> return $ MakeWorld board x y

lowerBound :: Int -> Int
lowerBound n = max n 0

upperBound :: Int -> Int
upperBound n = min n 1

drawWorld :: World -> IO ()
drawWorld (MakeWorld board x y) = do
	wclear stdScr
	wMove stdScr 0 0
	--foldl (\ys -> foldl (\c -> myshow (return ()) c) ys) (return ()) board
	board' <- return $ map (\ys -> map (show) ys) board
	board'' <- return $ foldr (++) "" $ foldr (\a b -> a ++ ["\r\n"] ++ b) [] board'
	putStr board''
	putStr $ "x: " ++ show x ++ " y: " ++ show y

myshow :: IO a -> Cell -> IO ()
myshow boo c = putStr $ show c

drawSpace :: [[Cell]] -> Int -> Int -> IO ()
drawSpace board x y = do
	wMove stdScr y x
	putStr $ show $ board !! x !! y
	


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

