import UI.HSCurses.Curses

type Health = Int
type Name = String

data Player = Player Name Health

data Cell = Wall
					| Empty
					| Goal

data World = MakeWorld [[Cell]] Int Int

initWorld :: IO World
initWorld = return $ MakeWorld [[Empty, Wall], [Empty, Goal]] 0 0

initPlayer :: IO Player
initPlayer = return $ Player "Someone" 10

act :: World -> Key -> IO World
act (MakeWorld board x y) i = case i of
	KeyChar 'h' -> return $ MakeWorld board (x - 1) y
	KeyChar 'j' -> return $ MakeWorld board x (y - 1) 
	KeyChar 'k' -> return $ MakeWorld board x (y + 1)
	KeyChar 'l' -> return $ MakeWorld board (x + 1) y
	otherwise -> return (MakeWorld board x y)

drawWorld :: World -> IO ()
drawWorld (MakeWorld board x y) = putStrLn $ "x: " ++ show x ++ " y: " ++ show y


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
	initCurses
	world <- initWorld
	player <- initPlayer
	gameLoop 0 world player
	putStr "Goodbye!"

