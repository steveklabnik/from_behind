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
	width <- return $ fst $ makeRandomValueST (4,15) (snd (next gen))
	height <- return $ fst $ makeRandomValueST (4,15) (snd (next gen))
	return $ MakeWorld (map (\_ -> map (genCell) [0,1 .. width]) [0,1 .. height]) initPlayer
	where
		genCell :: Int -> Cell
		genCell _ = case (fst (makeRandomValueST (1,4) gen)) :: Int of
			1 -> Wall
			otherwise -> Empty

makeRandomValueST :: (Int, Int) -> StdGen -> (Int, StdGen)
makeRandomValueST (x,y) = runState (getOne (x,y)) where
	getOne :: (Random a) => (a,a) -> State StdGen a
	getOne bounds = do 
		g      <- get
		(x,g') <- return $ randomR bounds g
		put g'
		return x



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
drawWorld (MakeWorld board (Player _ _ (x,y))) = do
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
hasWon (MakeWorld board (Player _ _ (x,y))) = board !! x !! y == Goal

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
	echo False
	nl False
	gen <- getStdGen
	world <- initWorld gen
	drawWorld world
	gameLoop 0 world
	endWin
	putStrLn "Goodbye!"

