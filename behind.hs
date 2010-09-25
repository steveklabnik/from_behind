import UI.HSCurses.Curses
import System.Random
import Control.Monad.State

type Health = Int
type Name = String

data Player = Player Name Health (Int, Int)

getPlayerX :: Player -> Int
getPlayerX (Player _ _ (x,_)) = x

getPlayerY :: Player -> Int
getPlayerY (Player _ _ (_,y)) = y

setPlayerXY :: Player -> Int -> Int -> Player 
setPlayerXY p x y = setPlayerX (setPlayerY p y) x

setPlayerX :: Player -> Int -> Player
setPlayerX (Player n h (_,y)) x = Player n h (x,y)

setPlayerY :: Player -> Int -> Player
setPlayerY (Player n h (x,_)) y = Player n h (x,y)

data Cell = Wall
          | Empty
          | Goal deriving (Eq) 

instance Show Cell where 
	show Wall = "#"
	show Empty = "."
	show Goal = "<"


data World = MakeWorld [[Cell]] Player

initWorld :: StdGen -> IO World
initWorld gen = do
	(width, gen) <- randomNum 3 75 gen
	(height, gen) <- randomNum 3 23 gen
	(goalX, gen) <- randomNum 2 width gen
	(goalY, gen) <- randomNum 2 height gen
	cells <- return $ map (genCell) (randoms gen :: [Int])
	board <- return $ (map (\i -> take width (drop (i * width) cells)) [0,1 .. height])
	board <- return $ addGoal goalX goalY board 
	return $ MakeWorld board initPlayer
		where
			genCell :: Int -> Cell
			genCell n = case x of
					0 -> Wall
					otherwise -> Empty
					where x = mod n 4

addGoal :: Int -> Int -> [[Cell]] -> [[Cell]]
addGoal x y (b:board)  
	| y == 1 = (addGoal' x b) : board
	| otherwise = b : addGoal x (y-1) board 
	where
		addGoal' :: Int -> [Cell] -> [Cell]
		addGoal' x (r:row) 
			| x == 1 = Goal : row
			|	otherwise = r : addGoal' (x-1) row
							

type GeneratorState = State StdGen

randomGen :: Int -> Int -> GeneratorState Int
randomGen min max = do 
	generator <- get
	let( value, newGenerator ) = randomR (min,max) generator
	put newGenerator
	return value

randomNum :: Int -> Int -> StdGen -> IO (Int, StdGen)
randomNum min max gen = return $ runState (randomGen min max) gen


initPlayer :: Player
initPlayer = Player "Someone" 10 (0,0)

act :: World -> Key -> IO World
act (MakeWorld board p) i  
    | board !! yi !! xi == Wall = return $ MakeWorld board p
    | otherwise = return $ MakeWorld board (setPlayerXY p xi yi)
    where
	x = getPlayerX p
	y = getPlayerY p
	(xi, yi) = checkBounds board $ case i of
		KeyChar 'h' -> (x-1, y)
		KeyChar 'j' -> (x, y+1)
		KeyChar 'k' -> (x, y-1)
		KeyChar 'l' -> (x+1, y)
		otherwise -> (x, y)

checkBounds :: [[Cell]] -> (Int, Int) -> (Int, Int)
checkBounds b (x, y) = (min (max x 0) ((length (b !! 0)) - 1), min (max y 0) ((length b) - 1))


drawWorld :: World -> IO ()
drawWorld (MakeWorld board (Player name hp (x,y))) = do
	wclear stdScr
	move 0 0
	wAddStr stdScr $ "HP: " ++ (show hp)
	move 1 0
	board' <- return $ foldr (++) "" $ foldr (\a b -> (map show a) ++ ["\n"] ++ b) [] board
	wAddStr stdScr board'
	wMove stdScr (y+1) x
	wAddStr stdScr "@"
	refresh

hasWon :: World -> Bool
hasWon (MakeWorld board (Player _ _ (x,y))) = board !! y !! x == Goal

gameLoop :: Int -> World -> IO World
gameLoop n w  
	| hasWon w == True = return w
	| otherwise = do 
		i <- getCh
		w' <- act w i
		drawWorld w'
		gameLoop (n + 1) w' 

main = do
	initCurses
	cBreak True
	cursSet CursorInvisible
	echo False
	nl False
	gen <- getStdGen
	world <- initWorld gen
	drawWorld world
	gameLoop 0 world
	endWin
	putStrLn "Goodbye!"

