import UI.HSCurses.Curses

type Health = Int
type Name = String

data Player = Player Name Health

data Cell = Wall
					| Empty
					| Goal deriving (Eq) 

instance Show Cell where 
	show Wall = "#"
	show Empty = "."
	show Goal = "<"


data World = MakeWorld [[Cell]] Int Int

initWorld :: IO World
initWorld = return $ MakeWorld [[Empty, Empty, Empty], [Empty, Goal, Empty], [Empty, Wall, Empty]] 0 0

initPlayer :: IO Player
initPlayer = return $ Player "Someone" 10

act :: World -> Key -> IO World
act (MakeWorld board x y) i  
    | board !! yi !! xi == Wall = return $ MakeWorld board x y
    | otherwise = return $ MakeWorld board xi yi
    where
	(xi, yi) = checkBounds board $ case i of
		KeyChar 'h' -> (x-1, y)
		KeyChar 'j' -> (x, y+1)
		KeyChar 'k' -> (x, y-1)
		KeyChar 'l' -> (x+1, y)
		otherwise -> (x, y)


checkBounds :: [[Cell]] -> (Int, Int) -> (Int, Int)
checkBounds b (x, y) = (min (max x 0) ((length (b !! 0)) - 1), min (max y 0) ((length b) - 1))


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
hasWon (MakeWorld board x y) = board !! x !! y == Goal

gameLoop :: Int -> World -> Player -> IO World
gameLoop n w p 
	| hasWon w == True = return w
	| otherwise = do 
		i <- getCh
		w' <- act w i
		drawWorld w'
		gameLoop (n + 1) w' p

main = do
	initCurses
	cBreak True
	echo False
	nl False
	world <- initWorld
	player <- initPlayer
	drawWorld world
	gameLoop 0 world player
	endWin
	putStr "Goodbye!"

