type World = String
type Player = String

initWorld :: IO World
initWorld = return "World"

initPlayer :: IO Player
initPlayer = return "Player"

act :: World -> String -> World
act w i = w

hasWon :: World -> Bool
hasWon w = True

getInput :: IO String
getInput = getLine

gameLoop :: Int -> World -> Player -> IO World
gameLoop n w p 
	| hasWon w == (n == 10) = return w
	| otherwise = do 
		i <- getInput
		putStrLn ("Hey " ++ show n ++ " " ++ i)
		hFlush
		gameLoop (n + 1) (act w i) p

main = do
	world <- initWorld
	player <- initPlayer
	gameLoop 0 world player
	putStr "Goodbye!"

