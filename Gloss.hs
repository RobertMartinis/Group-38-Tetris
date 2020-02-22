import Graphics.Gloss

type Block = [[Bool]]
type GridSquare = (Bool, Color)
type Field = [[GridSquare]]

{-
rotateBlock :: Block -> Block
rotateBlock ((a1, a2, a3, a4),
             (b1, b2, b3, b4),
	     (c1, c2, c3, c4),
	     (d1, d2, d3, d4)) = ((d1, c1, b1, a1),
                                  (d2, c2, b2, a2),
	                          (d3, c3, b3, a3),
	                          (d4, c4, b4, a4))


rotateBlockRight :: Block -> Block
rotateBlockRight ((a1, a2, a3, a4),
                  (b1, b2, b3, b4),
	          (c1, c2, c3, c4),
	          (d1, d2, d3, d4)) = ((a4, b4, c4, d4),
                                       (a3, b3, c3, d3),
	                               (a2, b2, c2, d2),
	                               (a1, b1, c1, d1))
-}
data GameState = Game { fallingBlock :: (Block,Color,(Int,Int)),
                        playField :: Field,
			tick :: Int
                      }



initialField :: Field
initialField = take 20 (repeat (take 10 (repeat (False,black))))

initialBlock = ([[True,True,True,False],
                 [False,True,False,False],
		 [False,False,False,False],
		 [False,False,False,False]],green,(7,15))


initialBlock'  = [[True,True,True,False],
                 [False,True,False,False],
		 [False,False,False,False],
		 [False,False,False,False]]
		 
initialBlock'' = [[False,False,False,False],
                 [False,False,True,False],
		 [False,False,False,False],
		 [False,False,False,False]]

initialGameState :: GameState
initialGameState = Game { fallingBlock = initialBlock,
                         playField = initialField,
			 tick = 1
		       }

fallStep :: GameState -> GameState
fallStep game = game {fallingBlock = (block, color, (x, newY))}
  where
    (block,color,(x,y)) = fallingBlock game
    newY = y - 1


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

    placeRow :: [Bool] -> [GridSquare] -> Color -> Int -> [GridSquare]
    placeRow _ [] _ _ = []
    placeRow [] ys _ _ = ys
    placeRow (x:xs) (y:ys) color 0 | x         = (True,color) : (placeRow xs ys color 0)
                                   | otherwise = y : (placeRow xs ys color 0)
    placeRow (x:xs) (y:ys) color xc = y : placeRow (x:xs) ys color (xc-1)
    
    place :: (Block, Color, (Int,Int)) -> [[GridSquare]] -> [[GridSquare]]
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

stringify' :: [GridSquare] -> String
stringify' [] = []
stringify' ((cell,color):xs) | cell = "x " ++ stringify' xs
                             | otherwise = "_ " ++ stringify' xs



-- | Render gamestate with Gloss

renderGame :: GameState -> Picture
renderGame game = pictures [
                    playfield--,fallingblock
		    ]
  where
    playfield = gridFromField (playField game) 0 --0 är accumulator som håller koll på y-koordinat/vilken rad
--makeColor8 (0,0,0,0) == transparent
--fallingblock =

    gridFromField :: Field -> Int -> Picture
    gridFromField (x:xs) 19 = rowOfSquares x 19 0
    gridFromField (x:xs) r  = pictures [
                                       (rowOfSquares x r 0), --denna 0a är accumulator som håller koll på x-koordinat/vilken kolumn
				       gridFromField xs (r+1)
				       ]

    rowOfSquares :: [GridSquare] -> Int -> Int -> Picture
    rowOfSquares (x:xs) r 9 = createSquare x (9,r)
    rowOfSquares (x:xs) r c = pictures [
                                       createSquare x (c,r),
				       rowOfSquares xs r (c+1)
				       ]

    createSquare :: GridSquare -> (Int,Int) -> Picture
    createSquare x (c,r) = translate (fromIntegral(c*30)) (fromIntegral(r*30)) $ color (snd x) $ translate (fromIntegral(-150)) (fromIntegral(-300)) $ polygon [(0,0), (0,30), (30,30), (30,0)]




-- | Get the 4x4 grid from playField where the fallingBlock is going to appear next step

nextBlockPos :: (Int,Int) -> Field -> Block
nextBlockPos (xc,0) ((x:xs):xss) = getNextRows xc xss (4::Int) --4 because we only want the next 4 rows
  where
    getNextRows :: Int -> Field -> Int -> Block
    getNextRows xc [] _ = []
    getNextRows xc _ 0 = []
    getNextRows xc (x:xs) n = (getInRow xc x 4) : (getNextRows xc xs (n-1))

    getInRow :: Int -> [GridSquare] -> Int -> [Bool]
    getInRow _ [] _ = []
    getInRow _ _ 0  = []
    getInRow 0 (x:xs) n  = (fst x) : getInRow 0 xs (n-1)
    getInRow xc (x:xs) n = getInRow (xc-1) xs n

nextBlockPos (xc,yc) ((x:xs):xss) = nextBlockPos (xc,(yc-1)) xss



-- | checks collision on next step

collision :: Block -> Block -> Bool  -- ^ first Block is from fallingBlock, second is from nextBlockPos
collision [] [] = False
collision ([]:xss) ([]:yss) = collision xss yss
collision ((x:xs):xss) ((y:ys):yss) = (x&&y) || (collision (xs:xss) (ys:yss))

-- | renders inital state of game

main = display (InWindow "hello" (300,600) (0,0)) white (renderGame ( placeBlock ( initialGameState)))







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
  --rotateBlock ?

  --annars
  --LeftButton
    --rotateLeft

  --RightButton
    --rotateRight

--Space
  --fallStep tills collision True, sen placeBlock



