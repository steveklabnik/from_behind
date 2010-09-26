import UI.HSCurses.Curses
import System.Random
import Control.Monad.State
import List

type Health = Int
type Pos = (Int, Int)

data Player = Player { playerName::String, 
											 playerHP::Int,
											 playerPos::Pos,
											 playerStr::Int,
											 playerWeapon::Item
										 }

data Cell = Wall
          | Empty
          | Goal deriving (Eq) 

data Item = Item { itemName::String,
									 itemPos::Pos,
									 itemFunc::(Player -> Player)
								 }

instance Eq Item where 
	(==) a b = (itemName a == itemName b) && (itemPos a == itemPos b)

instance Show Item where
	show (Item name pos func) = name

potion :: Pos -> Int -> Item
potion pos hp = Item { itemName = "Potion",
											 itemPos = pos,
											 itemFunc = f}
											where
												f p = p { playerHP = ((playerHP p) + hp) }

weapon :: String -> Pos -> Int -> Item
weapon name pos dmg = Item { itemName = name,
												itemPos = pos,
												itemFunc = f }
											where
												f p = p { playerWeapon = weapon name pos dmg }

instance Show Cell where 
	show Wall = "#"
	show Empty = "."
	show Goal = "<"


data World = World { worldBoard::[[Cell]],
										 worldItems::[Item],
										 worldPlayer::Player }
 

initWorld :: StdGen -> IO World
initWorld gen = do
	(width, gen) <- randomNum 3 75 gen
	(height, gen) <- randomNum 3 23 gen
	(goalX, gen) <- randomNum 2 width gen
	(goalY, gen) <- randomNum 2 height gen
	items <- return $ genItems (width, height) 5 (randoms gen :: [Int])
	cells <- return $ map (genCell) (randoms gen :: [Int])
	board <- return $ (map (\i -> take width (drop (i * width) cells)) [0,1 .. height])
	board <- return $ addGoal goalX goalY board 
	return $ World board items initPlayer
		where
			genItems :: (Int, Int) -> Int -> [Int] -> [Item]
			genItems (width, height) count (x:y:z:xs)
				| count == 0 = []
				| otherwise = (potion ((mod x width), (mod y height)) (mod z 10)) : (genItems (width, height) (count - 1) xs)
			genCell :: Int -> Cell
			genCell n = case x of
					0 -> Wall
					otherwise -> Empty
					where x = mod n 4
			initPlayer :: Player
			initPlayer = Player "Someone" 10 (0,0) 1 $ weapon "Fist" (0,0) 1  
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


act :: World -> Key -> IO World
act (World board items p) i  
    | board !! yi !! xi == Wall = return $ World board items p
		| itemList /= [] = return $ World board (items \\ itemList) (useItems (p {playerPos = (xi, yi)}))
    | otherwise = return $ World board items (p { playerPos = (xi, yi)} )
    where
	x = fst $ playerPos p
	y = snd $ playerPos p
	itemList = filter (checkItem (xi,yi)) items
	(xi, yi) = checkBounds board $ case i of
		KeyChar 'h' -> (x-1, y)
		KeyChar 'j' -> (x, y+1)
		KeyChar 'k' -> (x, y-1)
		KeyChar 'l' -> (x+1, y)
		KeyChar 'y' -> (x-1, y-1)
		KeyChar 'u' -> (x+1, y-1)
		KeyChar 'b' -> (x-1, y+1)
		KeyChar 'n' -> (x+1, y+1)
		otherwise -> (x, y)
	useItems :: Player -> Player
	useItems p = foldl (flip itemFunc) p itemList
	checkBounds :: [[Cell]] -> (Int, Int) -> (Int, Int)
	checkBounds b (x, y) = (min (max x 0) ((length (b !! 0)) - 1), min (max y 0) ((length b) - 1))
	checkItem :: (Int, Int) -> Item -> Bool
	checkItem (playerX, playerY) (Item name (x,y) _)
		| (x == playerX) && (y == playerY) = True
		| otherwise = False 

drawWorld :: World -> IO ()
drawWorld (World board items (Player name hp (x,y) _ w)) = do
	wclear stdScr
	move 0 0
	wAddStr stdScr $ name ++ " HP: " ++ (show hp) ++ " Weapon: " ++ (show w)
	move 1 0
	board' <- return $ foldr (++) "" $ foldr (\a b -> (map show a) ++ ["\n"] ++ b) [] board
	wAddStr stdScr board'
	drawItems items
	move (y+1) x
	wAddStr stdScr "@"
	refresh
	where
		drawItems :: [Item] -> IO ()
		drawItems [] = return ()
		drawItems (x:xs) = do
			drawItem x
			drawItems xs
		drawItem :: Item -> IO ()
		drawItem (Item name (x,y) _) = do
			move (y+1) x
			wAddStr stdScr "!"

gameLoop :: Int -> World -> IO World
gameLoop n w  
	| hasWon w == True = return w
	| otherwise = do 
		-- grab a key, do something with it
		i <- getCh
		w' <- act w i
		drawWorld w'
		gameLoop (n + 1) w' 
		where
			hasWon :: World -> Bool
			hasWon (World board _ (Player _ _ (x,y) _ _)) = board !! y !! x == Goal

main = do
	--tons of curses stuff
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

