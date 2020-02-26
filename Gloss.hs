import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import System.IO.Unsafe
import System.Random

type Block = [[Bool]]
type GridSquare = (Bool, Color)
type Field = [[GridSquare]]
type FieldRow = [GridSquare]
type Cords = (Int,Int)

-- Tetriminos

tBlock  = [[True,True,True,False],
           [False,True,False,False],
           [False,False,False,False],
           [False,False,False,False]]

iBlock = [[True,True,True,True],
          [False,False,False,False],
          [False,False,False,False],
          [False,False,False,False]]

oBlock = [[False,False,False,False],
          [False,True,True,False],
          [False,True,True,False],
          [False,False,False,False]]

jBlock = [[False,True,False,False],
          [False,True,False,False],
          [True,True,False,False],
          [False,False,False,False]]

lBlock = [[True,False,False,False],
          [True,False,False,False],
          [True,True,False,False],
          [False,False,False,False]]

sBlock = [[False,True,True,False],
          [True,True,False,False],
          [False,False,False,False],
          [False,False,False,False]]

zBlock = [[True,True,False,False],
          [False,True,True,False],
          [False,False,False,False],
          [False,False,False,False]]


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

data GameState = Game { fallingBlock :: (Block,Color,Cords),
                        playField :: Field,
			tick :: Int,
                        scoreCounter :: Int
                      }

lastRowTrue :: GameState -> GameState
lastRowTrue game = game {playField = newField}
  where
    field = playField game
    newField = changeLast field

    changeLast :: Field -> Field
    changeLast (x:[]) = [c,c,c,c,c,c,c,c,c,c] : []
    changeLast (x:xs) = x : (changeLast xs)

    c = (True,green)

initialField = take 21 (repeat (take 10 (repeat (False,black))))

{-
initialBlock = ([[False,False,False,False],
                 [False,True,True,False],
		 [False,True,False,False],
		 [False,True,False,False]],green,(7,0))
-}

initialGameState :: GameState
initialGameState = Game { fallingBlock = (tBlock,green,(7,1)),
                         playField = initialField,
			 tick = 0,
                         scoreCounter = 0
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
    --newblock =
    
    (block, color, (x,y)) = fallingBlock game
    field = playField game

    replaceNth :: Int -> a -> [a] -> [a]
    replaceNth _ _ [] = []
    replaceNth n newVal (x:xs)
      | n == 0 = newVal:xs
      | otherwise = x:replaceNth (n-1) newVal xs

    placeRow :: [Bool] -> FieldRow -> Color -> Int -> FieldRow
    placeRow _ [] _ _ = []
    placeRow [] ys _ _ = ys
    placeRow (x:xs) (y:ys) color 0 | x         = (True,color) : (placeRow xs ys color 0)
                                   | otherwise = y : (placeRow xs ys color 0)
    placeRow (x:xs) (y:ys) color xc = y : placeRow (x:xs) ys color (xc-1)
    
    place :: (Block, Color, Cords) -> Field -> Field
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
                            scorecounter,
         		    fallingblock,
                            playfield
		           ]
  where
    playfield = gridFromField (playField game) 0 --0 är accumulator som håller koll på y-koordinat/vilken rad
--makeColor8 (0,0,0,0) == transparent

    scorecounter = color white (Text (show (updateScore game)))

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

    gridFromBlock :: (Block,Color,Cords) -> Picture
    gridFromBlock ((a:b:c:d:xs),color,(xc,yc)) = pictures [blockRow a color (xc, yc),
                                                           blockRow b color (xc,(yc+1)),
							   blockRow c color (xc,(yc+2)),
							   blockRow d color (xc,(yc+3))]

    blockRow :: [Bool] -> Color -> Cords -> Picture
    blockRow (x:[]) color (xc,yc) = createSquare (x,color) (xc,yc)
    blockRow (x:xs) color (xc,yc) = pictures [(createSquare (x,color) (xc,yc)), (blockRow xs color ((xc+1),yc))]
    
    
    
    createSquare :: GridSquare -> Cords -> Picture
    createSquare x (c,r) = translate (fromIntegral(c*30)) (fromIntegral(-(r*30))) $ color (if (fst x) then (snd x) else (makeColorI 0 0 0 0)) $ translate (fromIntegral(-150)) (fromIntegral(300)) $ polygon [(0,0), (0,-30), (30,-30), (30,0)]




-- | Get the 4x4 grid from playField where the fallingBlock is going to appear next step

nextBlockPos :: Cords -> Field -> Block
nextBlockPos (xc,0) ((x:xs):xss) = getNextRows xc xss (4::Int) --4 because we only want the next 4 rows
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
    getInRow xc (x:xs) n = getInRow (xc-1) xs n

nextBlockPos (xc,yc) ((x:xs):xss) = nextBlockPos (xc,(yc-1)) xss



-- | checks collision on next step

collision :: Block -> Block -> Bool  -- ^ first Block is from fallingBlock, second is from nextBlockPos
collision [] [] = False
collision ([]:xss) ([]:yss) = collision xss yss
collision ((x:xs):xss) ((y:ys):yss) = (x&&y) || (collision (xs:xss) (ys:yss))

moveRows :: GameState -> Field
moveRows game = newField
  where
    field = playField game -- field = currentField
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

clearRows :: Field -> Field
clearRows [] = []
clearRows (x:xs) | row == x = clearRows xs 
                 | otherwise = x : clearRows xs
                  where
                   row = (isCleared 0 (x:xs))

-- Returns the row that is full and needs to be cleared
isCleared :: Int -> Field -> FieldRow
isCleared _ [] = []
isCleared yc (x:xs) | isCleared' x = x 
                    | otherwise = isCleared (yc+1) xs
                    where
                      -- Kollar om en rad är full
                      isCleared' :: FieldRow -> Bool
                      isCleared' [] = True
                      isCleared' (x:xs) | (fst(x)) = isCleared' xs
                                        | otherwise = False

resetBlock :: GameState -> GameState
resetBlock game = game {fallingBlock = (block,color,(2,1)), --Changed y coordinate to 1
                        playField = newField,
                        scoreCounter = newScore
                       }
  where
    (block,color,(x,_)) = fallingBlock game
    field = playField game
    newScore = updateScore game
    newField = moveRows game

    
updateScore :: GameState -> Int
updateScore game = newScore
  where
    field = playField game
    newScore = (scoreCounter game) + (rowsFull 0 field)
    
    rowsFull :: Int -> Field -> Int
    rowsFull score [] = score
    rowsFull score (x:xs) | rowsFull' x = rowsFull (score+10) xs
                          | otherwise = rowsFull score xs
    rowsFull' :: FieldRow -> Bool
    rowsFull' [] = True
    rowsFull' (x:xs) | (fst(x)) = rowsFull' xs
                     | otherwise = False
    

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

resetTick :: GameState -> GameState
resetTick game = game {tick = 0}

checkTick :: Int -> Bool
checkTick n = n > 19

tryMove :: GameState -> GameState
tryMove game = if (collision fallBlock nextPosInField) then
                 resetBlock $ placeBlock $ resetTick $ game
	       else
	         resetTick $ fallStep $ game
		 
		 where
		   (fallBlock,_,(x,y)) = fallingBlock game
		   nextPosInField = nextBlockPos (x,y) (playField game)

-- | detects events

event :: Event -> GameState -> GameState
event (EventKey (SpecialKey KeyUp)   (Down) _ _) game = rotateBlock $ increaseTick $ game
event (EventKey (SpecialKey KeyDown) (Down) _ _) game = tryMove game
event (EventKey (SpecialKey KeyRight) (Down) _ _) game = stepRight $ increaseTick $ game
event (EventKey (SpecialKey KeyLeft) (Down) _ _) game = stepLeft $ increaseTick $ game
event _ game = if (checkTick (tick game)) then
	         tryMove game
	       else
	         increaseTick game
event (EventKey (Char 'q') (Down) _ _) game = resetGame game -- Not working currently, compiles but pattern match is redundant
    
resetGame :: GameState -> GameState
resetGame game = initialGameState

time :: Float -> GameState -> GameState
time _ game = game


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
       (InWindow "Tetris" (500,600) (0,0))
       black
       60
       (lastRowTrue initialGameState)
       renderGame
       (event)
       (time)


testcollision = collision (fst' (fallingBlock initialGameState))
