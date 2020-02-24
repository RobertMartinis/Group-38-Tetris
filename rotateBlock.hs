import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

type Block = [[Bool]]
type GridSquare = (Bool, Color)
type Field = [[GridSquare]]

data GameState = Game { fallingBlock :: (Block,Color,(Int,Int)),
                        playField :: Field,
			tick :: Int
                      }


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
