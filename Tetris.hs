import Test.HUnit -- For test cases
import Graphics.Gloss -- For generating user interface and graphics to the game
import Graphics.Gloss.Interface.IO.Interact -- For detecting user inputs. Used to rotate the playing-block.

{- Represents a tetrimino in a grid. Which tetrimino is represented depends on the order of True or False values in the list.
INVARIANT: Block always contains 4 True values, and 12 False. 
-}
type Block = [[Bool]]

{- A cell in a grid. (True, Green) represents an filled cell with the color green. False represents an empty cell.
-}
type GridSquare = (Bool, Color)

{- The background grid for the game. 
    INVARIANT: Field contains 200 [GridSquare] representing a 10 * 20 grid.
-}
type Field = [[GridSquare]]

{- A row in the background playing-grid. 
   INVARIANT: ?
-}
type FieldRow = [GridSquare]

{- Coordinates in the playing grid. (x, y) denotes the x and y value of a cell in the grid.
   INVARIANT: x && y > 0 in Coord (x, y)
-}
type Coords = (Int,Int)

 
type FallBlock = (Block,Color,Coords)


{- randomBlock seed
Takes seed and returns a pseudo-random FallBlock depending on what number seed is
RETURNS: pseudo-random FallBlock
-}

randomBlock :: Int -> FallBlock
randomBlock seed = blockList !! (mod (seed*2) 7)
  where
    blockList = [tBlock,iBlock,oBlock,jBlock,lBlock,sBlock,zBlock]

-- Custom colors

purple = makeColorI 255 0 255 255
light_blue = makeColorI 0 100 255 255

{- Tetrominoes (All blocks consisting of 4 squares)
   They exist inside a 4x4 grid to make rotation easier
   They also have a color and a coordinate -}
    
tBlock  =  ([[False,False,True,False],
	     [False,True,True,False],
             [False,False,True,False],
             [False,False,False,False]],purple,(3,0))

iBlock = ([[False,False,True,False],
           [False,False,True,False],
           [False,False,True,False],
           [False,False,True,False]],light_blue,(3,0))

oBlock = ([[False,True,True,False],
           [False,True,True,False],
           [False,False,False,False],
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


{- rotateBlock game
Rotates fallingBlock one time clockwise in GameState
RETURNS: game with updated fallingBlock
-}

rotateBlock :: GameState -> GameState
rotateBlock game = game { fallingBlock = ((checkBlock block),color,(x,y))
                        }
  where

    {- checkBlock block
    checks if a block is an oBlock and if it is, returns the block, otherwise rotates the block clockwise
    RETURNS: rotated block unless block is an oBlock, then oBlock
    -}

    checkBlock :: Block -> Block
    checkBlock block | block == o_block = block
                     | otherwise = rotateBlock' block

    (o_block,_,_) = oBlock
    (block, color, (x,y)) = fallingBlock game
    
     {- RotateBlock' block
    rotates a block clockwise
    RETURNS: rotated block 
    -}

    rotateBlock' :: Block -> Block
    rotateBlock' [(a1:a2:a3:a4:[]),
                  (b1:b2:b3:b4:[]),
	          (c1:c2:c3:c4:[]),
	          (d1:d2:d3:d4:[])] = [[d1, c1, b1, a1],
                                       [d2, c2, b2, a2],
	                               [d3, c3, b3, a3],
	                               [d4, c4, b4, a4]]

{- GameState
  Contains all information that has to be known to display and play the game
-}

data GameState = Game { fallingBlock :: FallBlock,
                        nextBlock :: FallBlock,
                        playField :: Field,
                        tick :: Int,
                        scoreCounter :: Int,
                        lineCounter :: Int,
                        allowSwap :: Bool,
                        seed :: Int,
                        level :: Int
                      }

-- Initial playField constisting of a 10x20 grid 
initialField = take 20 $ repeat $ take 10 $ repeat (False,black)

{- initialGameState
  applies initial data to all values in GameState
  RETURNS: GameState with start up values for the main function
-}
initialGameState :: GameState
initialGameState = Game { fallingBlock = tBlock,
                          nextBlock = lBlock,
                          playField = initialField,
			  tick = 0,
                          scoreCounter = 0,
                          lineCounter = 0,
                          allowSwap = True,
			  seed = 0,
                          level = 1
			}

{- fallStep game
Changes the coords of fallingBlock in GameState
RETURNS: game with increased fallingBlock y-coordinate
-}
fallStep :: GameState -> GameState
fallStep game = game { fallingBlock = (block, color, (x, newY))
                     }
  where
    (block,color,(x,y)) = fallingBlock game
    newY = y + 1
    
{- stepRight game
Changes the coords of fallingBlock in GameState
RETURNS: game with increased fallingBlock x-coordinate
-}

stepRight :: GameState -> GameState
stepRight game = game { fallingBlock = (block, color, (newX, y))
                      }
  where
    (block,color,(x,y)) = fallingBlock game
    newX = x + 1
    
{- stepLeft game
Changes the coords of fallingBlock in GameState
RETURNS: game with decreased fallingBlock x-coordinate
-}
stepLeft :: GameState -> GameState
stepLeft game = game { fallingBlock = (block, color, (newX, y))
                     }
  where
    (block,color,(x,y)) = fallingBlock game
    newX = x - 1

{- placeBlock game
  places the current falling block onto the play field.
  PRE: y-coordinates of fallingBlock is positive
  RETURNS: updates playField in game with fallingBlock placed at its coordinates
-}

placeBlock :: GameState -> GameState
placeBlock game = game { playField = newField
                       }
  where
    (block, color, (x,y)) = fallingBlock game
    
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

{- renderGame game
Generates a Picture out of the current GameState
-}
renderGame :: GameState -> Picture
renderGame game = pictures [ playfield,
                             gridlines,
                             scorecounter,
                             linecounter,
                             level,
         		     fallingblock,
			     nextblock
		           ]
  where
    playfield = gridFromField (playField game) 0

    gridlines = pictures [(pictures (verticalLines 0 verticalLine)),(pictures (horizontalLines 0 horizontalLine))]

    verticalLines :: Int -> [Picture] -> [Picture]
    verticalLines 11 _ = []
    verticalLines n (x:xs) = translate (fromIntegral(-30*n)) 0 x : verticalLines (n+1) xs

    verticalLine = take 11 $ repeat $ color white $ Line [(150,-300),(150,300)]

    horizontalLines :: Int -> [Picture] -> [Picture]
    horizontalLines 21 _ = []
    horizontalLines n (x:xs) = translate 0 (fromIntegral(30*n)) x : horizontalLines (n+1) xs

    horizontalLine = take 21 $ repeat $ color white $ Line [(-150,-300),(150,-300)]

    scorecounter = translate (-300) 0 $ scale (0.2) (0.2) $ color white $ Text $ "Score: " ++  (show $ updateScore game)

    linecounter = translate (-300) (-50) $ scale (0.2) (0.2) $ color white $ Text $ "Lines: " ++  (show $ updateLines game)

    level = translate (-300) (-100) $ scale (0.2) (0.2) $ color white $ Text $ "Level: " ++  (show $ (div (lineCounter game) 5 ))
   
    gridFromField :: Field -> Int -> Picture
    gridFromField (x:xs) 19 = rowOfSquares x 19 0
    gridFromField (x:xs) r  = pictures [(rowOfSquares x r 0),
				        gridFromField xs (r+1)]

    rowOfSquares :: FieldRow -> Int -> Int -> Picture
    rowOfSquares (x:xs) r 9 = createSquare x (9,r)
    rowOfSquares (x:xs) r c = pictures [createSquare x (c,r),
				        rowOfSquares xs r (c+1)]

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
    createSquare x (c,r) = translate (fromIntegral(c*30)) (fromIntegral(-(r*30))) $ color (if (fst x) then (snd x) else (makeColorI 0 0 0 0)) $ translate (fromIntegral(-150)) (fromIntegral(300)) $ polygon [(2,-2), (27,-2), (27,-27), (2,-27)]

{- nextBlockPos (xc,yc) field
  get a 4x4 grid from the play field from the coordinates of the falling block
  PRE: yc is positive
  RETURNS: [[Bool]]/Block where the Bool is from first element in GridSquare in field/[[GridSquare]] in a 4x4 grid starting from coordinates xc and yc.
-}

-- VARIANT: yc

nextBlockPos :: Coords -> Field -> Block
nextBlockPos (xc,0) (xss) = getNextRows xc xss (4::Int) --4 because we only want the next 4 rows
  where
    
    {- getNextRows xc field n
       RETURNS: the first four rows of field, with four of their elements each starting from xc
    -}
    --VARIANT: length field and n

    getNextRows :: Int -> Field -> Int -> Block
    getNextRows xc [] 0 = []
    getNextRows xc [] n = [True,True,True,True] : (getNextRows xc [] (n-1))
    getNextRows xc _ 0 = []
    getNextRows xc (x:xs) n = (getInRow xc x 4) : (getNextRows xc xs (n-1))

    {- getInRow xc row n
      RETURNS: four Bool values from elements of row starting from index xc and forward. If at end of row, makes True.
    -}
    --VARIANT: length row and n
    
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




{- collision block1 block2
  PRE: block1 has same dimensions as block2
  RETURNS: True if any one element in block1 is True while block2 has element True in the same position
-}

-- VARIANT: length(concat(block1)) and length(concat(block2))

collision :: Block -> Block -> Bool
collision [] [] = False
collision ([]:xss) ([]:yss) = collision xss yss
collision ((x:xs):xss) ((y:ys):yss) = (x&&y) || (collision (xs:xss) (ys:yss))

{- moveRows game
  Checks if any rows need to be cleared if they are full and removes those rows from the field and adds that many new rows to the field
  RETURNS: game with rows of playField cleared and moved
-}
moveRows :: GameState -> GameState
moveRows game = game { playField = moveRows' (playField game)
                     }
  where
    {- moveRows' field
     Clears rows that are full from a Field and adds that many new ones
     RETURNS: field with updated rows
    -}
    moveRows' :: Field -> Field
    moveRows' field = addRow (rowsMissing 20 (clearRows field)) (clearRows field)

    {- rowsMissing n xs
    Checks how many rows are missing out of n rows
    RETURNS: n
    -}

    -- VARIANT: length xs
    
    rowsMissing :: Int -> Field -> Int
    rowsMissing n [] = n
    rowsMissing n (x:xs) = rowsMissing (n-1) xs
    
    {- addRow n Field
    Adds n amounts of empty rows with 10 elements to the top of a Field
    RETURNS: field with n amounts of empty rows
    -}

    -- VARIANT : n
    
    addRow :: Int -> Field -> Field
    addRow (-1) _ = []
    addRow n field = addRow (n-1) [(take 10 (repeat (False,black)))] ++ field

    {- clearRows xs
    Clears all rows where the first element of every tuple in a row is True
    PRE: Not all first elements in the tuples are True
    RETURNS: Field with n amount of less FieldRow
    -}

    -- VARIANT: length xs

    clearRows :: Field -> Field
    clearRows [] = []
    clearRows (x:xs) | row == x = clearRows xs 
                     | otherwise = x : clearRows xs
      where
        row = firstFullRow (x:xs)

    {- firstFullRow xs
    Returns the first row that has only True
    RETURNS: First x in xs that has only True elements in the tuples
    -}

    -- VARIANT: length xs

    firstFullRow :: Field -> FieldRow
    firstFullRow [] = []
    firstFullRow (x:xs) | isFull x = x 
                        | otherwise = firstFullRow xs

  {- isFull xs
Checks if a row has only True elements
RETURNS: True if all elements in xs is True or False if 1 or more element is False
-}

-- VARIANT: length xs

isFull :: FieldRow -> Bool
isFull [] = True
isFull (x:xs) | fst(x)    = isFull xs
              | otherwise = False

{- showLevel game
Shows the current level in the game
RETURNS: level in game
-}

showLevel :: GameState -> Int
showLevel game = level game

{- resetBlock game
Takes the next block as the current block and puts it at the top
Updates scoreCounter if any rows are cleared
Updates lineCounter if any lines are cleared
RETURNS: new fallingBlock, new nextBlock, updated scoreCounter, updated lineCounter, updated allowSwap in game
-}

resetBlock :: GameState -> GameState
resetBlock game = game { fallingBlock = nextBlock game,
                         nextBlock = randomBlock $ seed game,
                         scoreCounter = updateScore game,
                         lineCounter = updateLines game,
                         allowSwap = True
                       }
                 
{- updateScore game
Adds 10*(amount of rows cleared) to scoreCounter in game
RETURNS: updated scoreCounter in game
-}

updateScore :: GameState -> Int
updateScore game = newScore
  where
    field = playField game
    newScore = (scoreCounter game) + (fullRows 0 0 field) -- Both zeroes are accumilators
    
    {- fullRows score multiplier xs
     Checks how many rows in a field are Full (Represented by True values in the field), adds 10 to score and 1 to multiplier for each row that are full and multiplies score and multiplier together
     RETURNS: score*multiplier
     -}

    -- VARIANT: length xs
    
    fullRows :: Int -> Int -> Field -> Int
    fullRows score multiplier [] = score*multiplier -- multiplier gives more points if more than 1 row is cleared
    fullRows score multiplier (x:xs) | isFull x = fullRows (score+10) (multiplier+1) xs
                                     | otherwise = fullRows multiplier score xs

{- updateLines game
Updates lineCounter to match how many rows are cleared
RETURNS: updated lineCounter in game
-}

updateLines :: GameState -> Int
updateLines game = newLines
  where
    field = playField game
    newLines = (lineCounter game) + (fullRows 0 field)
    
    {- fullRows lines xs
     Checks how many rows in a field are Full (Represented by True values in the field)
     RETURNS: lines
     -}

    -- VARIANT: length xs
    
    fullRows :: Int -> Field -> Int
    fullRows lines [] = lines
    fullRows lines (x:xs) | isFull x = fullRows (lines+1) xs
                          | otherwise = fullRows lines xs

{- increaseTick game
Increases the tick to automatically move blocks
RETURNS: Increased tick in game by 1
-}

increaseTick :: GameState -> GameState
increaseTick game = game { tick = (tick game+1)
                         } 

{- increaseSeed game
Increases the seed to generate a pseudo-random number
RETURNS: Increased seed in game by 1
-}

increaseSeed :: GameState -> GameState
increaseSeed game = game { seed = (seed game+1)
                         }

{- resetTick game
Resets the tick to 0
RETURNS: tick in game as 0
-}

resetTick :: GameState -> GameState
resetTick game = game { tick = 0
                      }

{- checkTick game
The function is used to progress the game every 20 ticks
RETURNS: False if tick in game is less than 19 and True if tick in game is greater than 19 
-}

checkTick :: GameState -> Bool
checkTick game = tick game >= (30 - 3*(div (lineCounter game) 5))

{- tryRotate game
Checks if rotation is possible
RETURNS: updated playField and increasted tick in game if rotation is possible, else unchanged Playfield and increasted tick in game
-}
tryRotate :: GameState -> GameState
tryRotate game = if (collision rotatedBlock nextPosInField) then
                   increaseTick game
		 else
		   rotateBlock $ increaseTick game
		   
		   where
		     (rotatedBlock,_,(x,y)) = fallingBlock (rotateBlock game)
		     nextPosInField = nextBlockPos (x,y) (playField game)

{- instaPlace game
Places a block instantly where it would end up if you were to let it fall
RETURNS: updated playField and increasted tick in game if rotation is possible, else unchanged Playfield and increasted tick in game
-}

-- VARIANT: Amount of rows where fallingBlock in game does not collide with playField in game

instaPlace :: GameState -> GameState
instaPlace game = if (collision fallBlock nextPosInField) then
                     gameOver $ moveRows $ resetBlock $ placeBlock $ resetTick $ game
                   else
	             instaPlace (fallStep game)
		 where
		   (fallBlock,_,(x,y)) = fallingBlock game
		   nextPosInField = nextBlockPos (x,y+1) (playField game)

{- tryMoveDown game
Checks if a block can move down one step
RETURNS: updated playField in game 
-}

tryMoveDown :: GameState -> GameState
tryMoveDown game = if (collision fallBlock nextPosInField) then
                     gameOver $ moveRows $ resetBlock $ placeBlock $ resetTick $ game
                   else 
	             resetTick $ fallStep $ game
		 
		 where
		   (fallBlock,_,(x,y)) = fallingBlock game
		   nextPosInField = nextBlockPos (x,y+1) (playField game)

{- tryMoveLeft game
Checks if a block can move left one step
RETURNS: updated playField in game 
-}

tryMoveLeft :: GameState -> GameState
tryMoveLeft game = if (collision fallBlock nextPosInField) then 
                     increaseTick game
		   else
		     increaseTick $ stepLeft game
		     
		     where
		       (fallBlock,_,(x,y)) = fallingBlock game
		       nextPosInField = take 4 $ nextBlockPos (x-1,y) (playField game)

{- tryMoveRight game
Checks if a block can move right one step
RETURNS: updated playField in game 
-}

tryMoveRight :: GameState -> GameState
tryMoveRight game = if (collision fallBlock nextPosInField) then
                      increaseTick game
		    else
		      increaseTick $ stepRight game
		     
		      where
		        (fallBlock,_,(x,y)) = fallingBlock game
		        nextPosInField = take 4 $ nextBlockPos (x+1,y) (playField game)

{- swapBlock game
switches the current playing block with nextBlock
RETURNS: new fallingBlock, new nextBlock, updated allowSwap
-}

swapBlock :: GameState -> GameState
swapBlock game = game { fallingBlock = newBlock,
                        nextBlock = newBlock2,
                        allowSwap = False
                      }
                 where
                   newBlock = nextBlock game
                   newBlock2 = randomBlock $ seed game

{- gameOver game
Checks if top row has any blocks in it, and restarts the game if it does
RETURNS: updated playField in game
-}

gameOver :: GameState -> GameState
gameOver game | gameOver' (head(playField game)) = initialGameState
              | otherwise = game
              where

                {- gameOver' xs
                 Takes a fieldRow and if there are any full blocks in that row and returns True in that case, implying that the game should be over, otherwise False
                 RETURNS: Bool
                -}
               
                -- VARIANT: length xs
                 
                gameOver' :: FieldRow -> Bool
                gameOver' [] = False
                gameOver' (x:xs) | fst(x) = True
                                 | otherwise = gameOver' xs

-- Detects inputs
event :: Event -> GameState -> GameState
event (EventKey (SpecialKey KeyUp)   (Down) _ _) game = increaseSeed $ tryRotate game -- Rotates block
event (EventKey (SpecialKey KeyDown) (Down) _ _) game = increaseSeed $ tryMoveDown game -- Moves block down one step
event (EventKey (SpecialKey KeyRight)(Down) _ _) game = increaseSeed $ tryMoveRight game -- Moves block right one step
event (EventKey (SpecialKey KeyLeft) (Down) _ _) game = increaseSeed $ tryMoveLeft game -- Moves block left one step
event (EventKey (SpecialKey KeySpace) (Down) _ _) game = increaseSeed $ instaPlace game -- Places block instantly
event (EventKey (Char 'r') (Down) _ _) game = increaseSeed $ initialGameState -- Resets the game
event (EventKey (Char 'c') (Down) _ _) game = if allowSwap game then -- Swaps block if it's allowed
                                              swapBlock game
                                              else
                                              increaseTick game
event _ game = if (checkTick game) then
	         tryMoveDown game
	       else
	         increaseTick game

{- auto time game
Checks tick in game and if tick is greater than 19, the function calls for tryMoveDown, otherwise increases tick in game
RETURNS: updated fallingBlock in game or updated tick in game 
-}

auto :: Float -> GameState -> GameState
auto _ game = if (checkTick game) then
	         tryMoveDown game
	       else
	         increaseTick game

main = play
       FullScreen -- (InWindow "Tetris" (600,600) (0,0))
       black -- Background color
       60 -- Frames per second
       initialGameState 
       renderGame
       event -- Handles inputs
       auto 

-- Test cases:

test1 = TestCase $ assertEqual ("random block: ") (randomBlock 1) (([[False,True,True,False],[False,True,True,False],[False,False,False,False],[False,False,False,False]],yellow,(3,0)))

test2 =
  let tBlock_test = first $ fallingBlock $ rotateBlock initialGameState 
      t = True
      f = False
      first :: (a,b,c) -> a
      first (n,_,_) = n
  in
  TestCase $ assertEqual ("Rotate tBlock") (tBlock_test) ([[f,f,f,f],[f,f,t,f],[f,t,t,t],[f,f,f,f]]) 

test3 =
  let blockDownOneStep = getY $ fallingBlock $ fallStep initialGameState
      getY :: (a,b,(c,c)) -> c
      getY (_,_,(_,yc)) = yc
  in
    TestCase $ assertEqual ("Fall step: ") (blockDownOneStep) (1)

test4 =
  let blockRightOneStep = getX $ fallingBlock $ stepRight initialGameState
      getX :: (a,b,(c,c)) -> c
      getX (_,_,(xc,_)) = xc
  in
    TestCase $ assertEqual ("Fall step: ") (blockRightOneStep) (4)

test5 =
  let blockLeftOneStep = getX $ fallingBlock $ stepLeft initialGameState
      getX :: (a,b,(c,c)) -> c
      getX (_,_,(xc,_)) = xc
  in
    TestCase $ assertEqual ("Fall step: ") (blockLeftOneStep) (2)

test6 = let
  (xc,yc) = (3,0)
  field = playField (placeBlock initialGameState)
  f = False
  t = True
  in
    TestCase $ assertEqual ("next block position: ") (nextBlockPos (3,0) (field)) ([[f,f,t,f],[f,t,t,f],[f,f,t,f],[f,f,f,f]])

test7 = let
  t = True
  f = False
  in
    TestCase $ assertEqual ("collision: ") (collision ([[f,f,f,f],[f,f,t,f],[f,f,f,f],[f,f,f,f]]) ([[f,f,f,f],[t,t,t,f],[t,f,t,t],[f,f,f,f]])) (True)

test8 = let
  gb = (True,green) -- green Block
  bb = (False,black) -- black Block
  tGameState = Game {playField = take 10(repeat gb) : take 19 (repeat (take 10(repeat bb)))}
  in
    TestCase $ assertEqual ("move rows: ") (playField $ moveRows tGameState) (playField initialGameState)

test9 = TestCase $ assertEqual ("is full: ") (isFull (head(initialField))) (False)

test10 = TestCase $ assertEqual ("show level: ") (showLevel initialGameState) (1)

test11 = let
  gameToThings :: GameState -> (FallBlock,FallBlock,Int,Int,Bool)
  gameToThings game = ((fallingBlock game),(nextBlock game),(scoreCounter game),(lineCounter game),(allowSwap game))
  tGameState = Game {fallingBlock = lBlock, nextBlock = tBlock, scoreCounter = 0, lineCounter = 0, allowSwap = True} --tGameState is the GameState is the next GameState if you resetBlock once
  in
    TestCase $ assertEqual ("reset block: ") (gameToThings (resetBlock initialGameState)) (gameToThings (tGameState))

test12 = TestCase $ assertEqual ("update score: ") (updateScore initialGameState) (0)

test13 = TestCase $ assertEqual ("update lines: ") (updateLines initialGameState) (0)

test14 = TestCase $ assertEqual ("increase tick: ") (tick (increaseTick initialGameState)) (1)

test15 = TestCase $ assertEqual ("increase seed: ") (seed (increaseSeed initialGameState)) (1)

test16 = TestCase $ assertEqual ("reset tick: ") (tick (resetTick initialGameState)) (tick initialGameState)

test17 = TestCase $ assertEqual ("check tick: ") (checkTick initialGameState) (False)

test18 = let
  pb = (True,purple) -- purple block
  bb = (False,black) -- black block
  field = take 17 (repeat (take 10(repeat bb))) ++ [[bb,bb,bb,bb,bb,pb,bb,bb,bb,bb]]++ [[bb,bb,bb,bb,pb,pb,bb,bb,bb,bb]] ++ [[bb,bb,bb,bb,bb,pb,bb,bb,bb,bb]] -- Field after instaplaced tBlock
  in
    TestCase $ assertEqual ("insta place: ") (playField $ instaPlace initialGameState) (field)

test19 = let
  gameToThings :: GameState -> (FallBlock,FallBlock,Bool)
  gameToThings game = ((fallingBlock game),(nextBlock game),(allowSwap game))
  tGameState = Game {fallingBlock = lBlock, nextBlock = tBlock, allowSwap = False}
  in
    TestCase $ assertEqual ("swap block: ") (gameToThings $ swapBlock initialGameState) (gameToThings tGameState)

test20 = let
  pb = (True,purple) -- purple block
  bb = (False,black) -- black block
  tGameState = Game {playField = [[bb,bb,bb,pb,bb,bb,pb,pb,bb,bb]] ++ take 19 (repeat (take 10(repeat bb)))}
  in
    TestCase $ assertEqual ("gameOver: ") (playField $ gameOver tGameState) (playField initialGameState)

runtests = runTestTT $ TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20]
