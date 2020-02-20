import Graphics.Gloss

type Block = [[Bool]]



type GridSquare = (Bool, Color)
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
initialField :: [[GridSquare]]
initialField = take 20 (repeat (take 10 (repeat (False,black))))

data GameState = Game { fallingBlock :: (Block,Color,(Int,Int)),
                        playField :: [[GridSquare]],
			tick :: Int
                      }

fallStep :: GameState -> GameState
fallStep game = game {fallingBlock = (block, color, (x, newY))}
  where
    (block,color,(x,y)) = fallingBlock game
    newY = y - 1

testpics = pictures [pictures [Circle 20, Circle 10], pictures [Circle 30, Circle 40]]
mergePics :: Picture -> Picture -> Picture

mergePics a b = pictures[a, b]

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

    placeRow :: [Bool] -> [GridSquare] -> Color -> [GridSquare]
    placeRow _ [] _ = []
    placeRow (x:xs) (y:ys) color | x = (True,color):(placeRow xs ys color)
                                 | otherwise = y:(placeRow xs ys color)
    
    place :: (Block, Color, (Int,Int)) -> [[GridSquare]] -> [[GridSquare]]
    place (block,color,(xc,0)) (a:[]) =
      (placeRow (block!!0) a color: [])
      
    place (block,color,(xc,0)) (a:b:[]) =
      (placeRow (block!!0) a color:
         placeRow (block!!1) b color: [])
	 
    place (block,color,(xc,0)) (a:b:c:[]) =
      (placeRow (block!!0) a color:
         placeRow (block!!1) b color:
	   placeRow (block!!2) c color: [])
	   
    place (block,color,(xc,0)) (a:b:c:d:xs) =
      (placeRow (block!!0) a color:
         placeRow (block!!1) b color:
           placeRow (block!!2) c color:
             placeRow (block!!3) d color: [])
	     
    place (block,color,(xc,yc)) (x:xs) = x : place (block,color,(xc,yc-1)) xs


    newField = place (fallingBlock game) (playField game)

