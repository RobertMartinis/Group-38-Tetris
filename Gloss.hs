
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import System.Random
import System.Exit

type Block = [[Bool]]
type GridSquare = (Bool, Color)
type Field = [[GridSquare]]
type FieldRow = [GridSquare]
type Coords = (Int,Int)
type FallBlock = (Block,Color,Coords)

{-
randomBlock :: [Block] -> Block
randomBlock l = do
  r <- show(getLine(randomRIO (0,7)))
  randomBlock' r l
  where
    randomBlock' :: Int -> [Block] -> Block
    randomBlock' 0 (x:xs) = x
    randomBlock' r (x:xs) = randomBlock' (r-1) xs
-}

{-
randBlock = randBlock' [tBlock,iBlock,oBlock,jBlock,lBlock,sBlock,zBlock]
  where
    randBlock' :: [a] -> IO a
    randBlock' list = do
      r <- randomRIO (0, length list - 1)
      return $ list !! r
-}
randomBlock :: Int -> FallBlock
randomBlock seed = blockList !! (mod (seed*2) 7 )
  where
    blockList = [tBlock,iBlock,oBlock,jBlock,lBlock,sBlock,zBlock]
    
-- Tetriminos
tBlock  =  ([[False,False,False,False],
	   [True,True,True,False],
           [False,True,False,False],
           [False,False,False,False]],makeColorI 255 0 255 255,(3,0))

iBlock = ([[False,False,True,False],
          [False,False,True,False],
          [False,False,True,False],
          [False,False,True,False]],makeColorI 0 100 255 255,(3,0))

oBlock = ([[False,False,False,False],
          [False,True,True,False],
          [False,True,True,False],
          [False,False,False,False]],yellow,(3,0))

lBlock = ([[False,True,False,False],
          [False,True,False,False],
          [False,True,True,False],
          [False,False,False,False]],dark blue, (3,0))

jBlock = ([[False,False,True,False],
          [False,False,True,False],
          [False,True,True,False],
          [False,False,False,False]],orange,(3,0))

zBlock = ([[False,False,True,False],
          [False,True,True,False],
          [False,True,False,False],
          [False,False,False,False]],red,(3,0))

sBlock = ([[False,True,False,False],
          [False,True,True,False],
          [False,False,True,False],
          [False,False,False,False]],green,(3,0))


rotateBlock :: GameState -> GameState
rotateBlock game = game { fallingBlock = (newBlock,color,(x,y))
                        }
  where
    (block, color, (x,y)) = fallingBlock game
    newBlock = rotateBlock' block
    
    rotateBlock' :: Block -> Block
    rotateBlock' [(a1:a2:a3:a4:[]),
                  (b1:b2:b3:b4:[]),
	          (c1:c2:c3:c4:[]),
	          (d1:d2:d3:d4:[])] = [[d1, c1, b1, a1],
                                       [d2, c2, b2, a2],
	                               [d3, c3, b3, a3],
	                               [d4, c4, b4, a4]]

data GameState = Game { fallingBlock :: FallBlock,
			nextBlock :: FallBlock,
                        playField :: Field,
			tick :: Int,
                        scoreCounter :: Int,
			seed :: Int
		      }

initialField = take 20 (repeat (take 10 (repeat (False,black))))

initialGameState :: GameState
initialGameState = Game { fallingBlock = tBlock,
                          nextBlock = lBlock,
                          playField = initialField,
			  tick = 0,
                          scoreCounter = 0,
			  seed = 0
			}

fallStep :: GameState -> GameState
fallStep game = game {fallingBlock = (block, color, (x, newY))}
  where
    (block,color,(x,y)) = fallingBlock game
    newY = y + 1
    
stepRight :: GameState -> GameState
stepRight game = game {fallingBlock = (block, color, (newX, y))}
  where
    (block,color,(x,y)) = fallingBlock game
    newX = x + 1
    
stepLeft :: GameState -> GameState
stepLeft game = game {fallingBlock = (block, color, (newX, y))}
  where
    (block,color,(x,y)) = fallingBlock game
    newX = x - 1

placeBlock :: GameState -> GameState
placeBlock game = game {
                        playField = newField
                       }
  where
    (block, color, (x,y)) = fallingBlock game
    field = playField game
    
    placeRow :: [Bool] -> FieldRow -> Color -> Int -> FieldRow
    placeRow _ [] _ _ = []
    placeRow [] ys _ _ = ys
    placeRow (x:xs) (y:ys) color 0 | x         = (True,color) : (placeRow xs ys color 0)
                                   | otherwise = y : (placeRow xs ys color 0)
    placeRow (x:xs) (y:ys) color xc = if xc > 0 then
                                        y : placeRow (x:xs) ys color (xc-1)
			              else
				        placeRow xs (y:ys) color (xc+1)
    
    place :: (Block, Color, Coords) -> Field -> Field
    place _ [] = []
    place (block,color,(xc,0)) (a:[]) =
      (placeRow (block!!0) a color xc: [])
      
    place (block,color,(xc,0)) (a:b:[]) =
      (placeRow (block!!0) a color xc:
         placeRow (block!!1) b color xc: [])
	 
    place (block,color,(xc,0)) (a:b:c:[]) =
      (placeRow (block!!0) a color xc:
         placeRow (block!!1) b color xc:
	   placeRow (block!!2) c color xc: [])
	   
    place (block,color,(xc,0)) (a:b:c:d:xs) =
      (placeRow (block!!0) a color xc:
         placeRow (block!!1) b color xc:
           placeRow (block!!2) c color xc:
             placeRow (block!!3) d color xc: xs) 
	     
    place (block,color,(xc,yc)) (x:xs) = x : place (block,color,(xc,yc-1)) xs


    newField = place (fallingBlock game) (playField game)

-- | Render the play field with text in console

stringify :: Field -> IO () 
stringify (x:[]) = putStrLn (stringify' x) 
stringify (x:xs) = do
  putStrLn (stringify' x)
  stringify xs

stringify' :: FieldRow -> String
stringify' [] = []
stringify' ((cell,color):xs) | cell = "x " ++ stringify' xs
                             | otherwise = "_ " ++ stringify' xs	

fst' (a,_,_) = a
trd' (_,_,a) = a

-- | Render gamestate with Gloss

renderGame :: GameState -> Picture
renderGame game = pictures [
                            verticalLines,
                            horizontalLines,
                            scorecounter,
         		    fallingblock,
			    nextblock,
                            playfield
		           ]
  where
    playfield = gridFromField (playField game) 0 --0 är accumulator som håller koll på y-koordinat/vilken rad
--makeColor8 (0,0,0,0) == transparent
    
    verticalLines = pictures (verticalLines' 0 verticalLine)

    verticalLines' :: Int -> [Picture] -> [Picture]
    verticalLines' 11 _ = []
    verticalLines' n (x:xs) = translate (fromIntegral(-30*n)) 0 x : verticalLines' (n+1) xs

    verticalLine = take 11 (repeat (color white (Line [(150,-600),(150,600)])))

    horizontalLines = pictures (horizontalLines' 0 horizontalLine)

    horizontalLines' :: Int -> [Picture] -> [Picture]
    horizontalLines' 20 _ = []
    horizontalLines' n (x:xs) = translate 0 (fromIntegral(30*n)) x : horizontalLines' (n+1) xs

    horizontalLine = take 20 (repeat (color white (Line [(-150,-300),(150,-300)])))

    scorecounter = translate (-300) 0 (scale (0.2) (0.2) (color white (Text ("Score: " ++ (show (updateScore game))))))

    gridFromField :: Field -> Int -> Picture
    gridFromField (x:xs) 19 = rowOfSquares x 19 0
    gridFromField (x:xs) r  = pictures [
                                       (rowOfSquares x r 0), --denna 0a är accumulator som håller koll på x-koordinat/vilken kolumn
				       gridFromField xs (r+1)
				       ]

    rowOfSquares :: FieldRow -> Int -> Int -> Picture
    rowOfSquares (x:xs) r 9 = createSquare x (9,r)
    rowOfSquares (x:xs) r c = pictures [
                                       createSquare x (c,r),
				       rowOfSquares xs r (c+1)
				       ]

    fallingblock = gridFromBlock (fallingBlock game)

    nextblock = gridFromBlock (nextBlockGrid,nextBlockColor,(13,0))
    (nextBlockGrid,nextBlockColor,_) = nextBlock game

    gridFromBlock :: (Block,Color,Coords) -> Picture
    gridFromBlock ((a:b:c:d:xs),color,(xc,yc)) = pictures [blockRow a color (xc, yc),
                                                           blockRow b color (xc,(yc+1)),
							   blockRow c color (xc,(yc+2)),
							   blockRow d color (xc,(yc+3))]

    blockRow :: [Bool] -> Color -> Coords -> Picture
    blockRow (x:[]) color (xc,yc) = createSquare (x,color) (xc,yc)
    blockRow (x:xs) color (xc,yc) = pictures [(createSquare (x,color) (xc,yc)), (blockRow xs color ((xc+1),yc))]
    
    createSquare :: GridSquare -> Coords -> Picture
    createSquare x (c,r) = translate (fromIntegral(c*30)) (fromIntegral(-(r*30))) $ color (if (fst x) then (snd x) else (makeColorI 0 0 0 0)) $ translate (fromIntegral(-150)) (fromIntegral(300)) $ polygon [(0,0), (0,-30), (30,-30), (30,0)]




-- | Get the 4x4 grid from playField where the fallingBlock is going to appear next step

nextBlockPos :: Coords -> Field -> Block
nextBlockPos (xc,0) (xss) = getNextRows xc xss (4::Int) --4 because we only want the next 4 rows
  where
    getNextRows :: Int -> Field -> Int -> Block
    getNextRows xc [] 0 = []
    getNextRows xc [] n = [True,True,True,True] : (getNextRows xc [] (n-1))
    getNextRows xc _ 0 = []
    getNextRows xc (x:xs) n = (getInRow xc x 4) : (getNextRows xc xs (n-1))

    getInRow :: Int -> FieldRow -> Int -> [Bool]
    getInRow _ [] 0 = []
    getInRow _ [] n = True : (getInRow 0 [] (n-1))
    getInRow _ _ 0  = []
    getInRow 0 (x:xs) n  = (fst x) : getInRow 0 xs (n-1)
    getInRow xc (x:xs) n = if xc > 0 then
                             getInRow (xc-1) xs n
			   else
			     True : getInRow (xc+1) (x:xs) (n-1)

nextBlockPos (xc,yc) ((x:xs):xss) = nextBlockPos (xc,(yc-1)) xss

-- | checks collision on next step

collision :: Block -> Block -> Bool  -- ^ first Block is from fallingBlock, second is from nextBlockPos
collision [] [] = False
collision ([]:xss) ([]:yss) = collision xss yss
collision ((x:xs):xss) ((y:ys):yss) = (x&&y) || (collision (xs:xss) (ys:yss))

-- Add new rows
moveRows :: GameState -> Field
moveRows game = newField
  where
    field = playField game
    newField = moveRows' field
    
    moveRows' :: Field -> Field
    moveRows' field = addRow (rowsMissing 20 (clearRows field)) (clearRows field)
    -- Checks how many rows are missing
    rowsMissing :: Int -> Field -> Int
    rowsMissing n [] = n
    rowsMissing n (x:xs) = rowsMissing (n-1) xs
    -- Adds n amount of rows 
    addRow :: Int -> Field -> Field
    addRow (-1) _ = []
    addRow n field = addRow (n-1) [(take 10 (repeat (False,black)))] ++ field

-- Empties all Full Rows
clearRows :: Field -> Field
clearRows [] = []
clearRows (x:xs) | row == x = clearRows xs 
                 | otherwise = x : clearRows xs
                  where
                   row = (isCleared 0 (x:xs))

-- Returns the row that is full and needs to be cleared
isCleared :: Int -> Field -> FieldRow
isCleared _ [] = []
isCleared yc (x:xs) | fullRow x = x 
                    | otherwise = isCleared (yc+1) xs

-- Checks if a given row is Full
fullRow :: FieldRow -> Bool
fullRow [] = True
fullRow (x:xs) | (fst(x)) = fullRow xs
               | otherwise = False

-- New block
resetBlock :: GameState -> GameState
resetBlock game = game { fallingBlock = nextBlock game,
                         nextBlock = randomBlock (seed game),    --(block,color,(2,0)),
                         playField = newField,
                         scoreCounter = newScore
                       }
  where
    (block,color,(x,_)) = fallingBlock game
    field = playField game
    newScore = updateScore game
    newField = moveRows game

-- Takes gameState and gives 10 points * (amount of rows cleared) per row
updateScore :: GameState -> Int
updateScore game = newScore
  where
    field = playField game
    newScore = (scoreCounter game) + (fullRows 0 0 field) -- Both zeroes are accumilators
    
    fullRows :: Int -> Int -> Field -> Int
    fullRows score multiplier [] = score*multiplier -- multiplier gives more points if more than 1 row is cleared
    fullRows score multiplier (x:xs) | fullRow x = fullRows (score+10) (multiplier+1) xs
                                     | otherwise = fullRows multiplier score xs

-- Checks if game is over and if it is not, clears rows that are full (if there are any)
{-
gameOver :: Field -> Field
gameOver field | gameOver' field = initialField
               | otherwise = moveRows 
               where
                 -- Checks if the top row is full
                 gameOver' :: GameState -> Bool
                 gameOver' game = game {playField} fullRow x
                 -- Checks if a row has a Full block
                 fullRow :: [GridSquare] -> Bool
                 fullRow [] = False
                 fullRow ((bool,_):xs) | bool == True = True
                                       | otherwise = fullRow xs
-}
increaseTick :: GameState -> GameState
increaseTick game = game {tick = (n+1)}
  where
    n = tick game

increaseSeed :: GameState -> GameState
increaseSeed game = game {seed = (n+1)}
  where
    n = seed game

resetTick :: GameState -> GameState
resetTick game = game {tick = 0}

checkTick :: Int -> Bool
checkTick n = n > 19

tryRotate :: GameState -> GameState
tryRotate game = if (collision rotatedBlock nextPosInField) then
                   increaseTick game
		 else
		   rotateBlock $ increaseTick game
		   
		   where
		     (rotatedBlock,_,(x,y)) = fallingBlock (rotateBlock game)
		     nextPosInField = nextBlockPos (x,y) (playField game)

tryMoveDown :: GameState -> GameState
tryMoveDown game = if (collision fallBlock nextPosInField) then
                     resetBlock $ placeBlock $ resetTick $ game
                   else
	             resetTick $ fallStep $ game
		 
		 where
		   (fallBlock,_,(x,y)) = fallingBlock game
		   nextPosInField = nextBlockPos (x,y+1) (playField game)

tryMoveLeft :: GameState -> GameState
tryMoveLeft game = if (collision fallBlock nextPosInField) then
                     increaseTick game
		   else
		     increaseTick $ stepLeft game
		     
		     where
		       (fallBlock,_,(x,y)) = fallingBlock game
		       nextPosInField = take 4 (nextBlockPos (x-1,y) (playField game))

tryMoveRight :: GameState -> GameState
tryMoveRight game = if (collision fallBlock nextPosInField) then
                      increaseTick game
		    else
		      increaseTick $ stepRight game
		     
		      where
		        (fallBlock,_,(x,y)) = fallingBlock game
		        nextPosInField = take 4 (nextBlockPos (x+1,y) (playField game))


-- Swaps block
swapBlock :: GameState -> GameState
swapBlock game = game {fallingBlock = newBlock,
                      nextBlock = newBlock2
                      }
                 where
                   newBlock = nextBlock game
                   newBlock2 = randomBlock (seed game)

-- | detects events

event :: Event -> GameState -> GameState
event (EventKey (SpecialKey KeyUp)   (Down) _ _) game = increaseSeed $ tryRotate game
event (EventKey (SpecialKey KeyDown) (Down) _ _) game = increaseSeed $ tryMoveDown game
event (EventKey (SpecialKey KeyRight)(Down) _ _) game = increaseSeed $ tryMoveRight game
event (EventKey (SpecialKey KeyLeft) (Down) _ _) game = increaseSeed $ tryMoveLeft game
event (EventKey (Char 'r') (Down) _ _) game = increaseSeed $ initialGameState
event (EventKey (Char 's') (Down) _ _) game = swapBlock game
event _ game = if (checkTick (tick game)) then
	         tryMoveDown game
	       else
	         increaseTick game
                 
time :: Float -> GameState -> GameState
time _ game = if (checkTick (tick game)) then
	         tryMoveDown game
	       else
	         increaseTick game

--createVertical :: Picture -> [Picture]
--createVertical 
--createHorizontal :: Picture -> [Picture]

--olika event

--Tick --om inget trycks ned
  --increaseTick
  --collision
    --fallStep
    --
    --placeBlock
    --
    --resetTick

--DownButton
  --collision
    --fallStep
    --
    --placeBlock
    --
    --resetTick

--UpButton
  --rotateBlock 

--Space
  --fallStep tills collision True, sen placeBlock



main = play
       FullScreen--(InWindow "Tetris" (600,600) (0,0))
       black
       60
       (initialGameState)
       renderGame
       (event)
       (time)

