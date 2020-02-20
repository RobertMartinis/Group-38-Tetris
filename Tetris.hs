import Graphics.Gloss

main :: IO ()
main = display window white (pictures [(pictures tBlock),(translate 100 0(pictures iBlock))])



type Square = [Picture]

type GridSquare = (Cell, Color)

data Cell = Empty | Full deriving (Show)


type Block = ((Cell, Cell, Cell, Cell),
              (Cell, Cell, Cell, Cell),
              (Cell, Cell, Cell, Cell),
              (Cell, Cell, Cell, Cell)) 


data GameState = Game
  { fallingBlock :: (Block, Color, (Int, Int)),
   playField :: [[GridSquare]]
   --tick :: Int
  }

-- clearRow

--

--picturify :: [[GridSquare]] ->

initialField :: [[GridSquare]]
initialField = take 20 (repeat (take 10 (repeat (Empty,black))))



fallStep :: GameState -> GameState
fallStep game = game {fallingBlock = (block, color, (x, newY))}
  where
    (block, color, (x,y)) = fallingBlock game
    newY = y - 1

moveRight :: GameState -> GameState
moveRight game = game {fallingBlock = (block, color, (newX, y))}
  where
    (block, color, (x,y)) = fallingBlock game
    newX = x + 1

moveLeft :: GameState -> GameState
moveLeft game = game {fallingBlock = (block, color, (newX, y))}
  where
    (block,color,(x,y)) = fallingBlock game
    newX = x - 1

fst' :: (a, a, a, a) -> a
fst' (_,_,a,_) = a

snd' :: (a, a, a, a) -> a
snd' (_,_,a,_) = a

trd' :: (a, a, a, a) -> a
trd' (_,_,a,_) = a

fth' :: (a, a, a, a) -> a
fth' (_,_,_,a) = a

rotateRight :: Block -> Block
rotateRight ((a1, a2, a3, a4),
             (b1, b2, b3, b4),
             (c1, c2, c3, c4),
             (d1, d2, d3, d4)) = ((d1, c1, b1, a1),
                                  (d2, c2, b2, a2),
                                  (d3, c3, b3, a3),
                                  (d4, c4, b4, a4))

rotateLeft :: Block -> Block
rotateLeft  ((a1, a2, a3, a4),
             (b1, b2, b3, b4),
             (c1, c2, c3, c4),
             (d1, d2, d3, d4)) = ((a4, b4, c4, d4),
                                  (a3, b3, c3, d3),
                                  (a2, b2, c2, d2),
                                  (a1, b1, c1, d1))

 {-
tBlock_test = ((False, False, False, False),
               
               (False, False, False, False),
               
               (False, True, True, True),
               
               (False, False, True, False))
-}
translate' :: Block -> IO ()
translate' (a,b,c,d) = do
  putStrLn (show(a))
  putStrLn (show(b))
  putStrLn (show(c))
  putStrLn (show(d))


gridUnit = 20

square = polygon [
  (0,0),
  (0,20),
  (20,20),
  (20,0)
  ]

--blockGrid = color white ()
{-
tBlock = color magenta (polygon [(0,50),(75,50),(75,25),(50,25),(50,0),(25,0),(25,25),(0,25)])
iBlock = color cyan (polygon [(0,0),(100,0),(100,25),(0,25)])
oBlock = color yellow (polygon [(0,0),(50,0),(50,50),(0,50)])
lBlock = color orange (polygon [(0,0),(50,0),(50,25),(25,25),(25,75),(0,75)])
jBlock = color blue (polygon [(0,0),(50,0),(50,75),(25,75),(25,25),(0,25)])
sBlock = color green (polygon [(0,0),(50,0),(50,25),(75,25),(75,50),(25,50),(25,25),(0,25)])
zBlock = color red (polygon [(0,25),(25,25),(25,0),(75,0),(75,25),(50,25),(50,50),(0,50)])
-}

{-
tBlock ...
        .

iblock ....

oBlock ..
       ..

jblock   .
         .
       ...

lblock .
       .
       ...

sblock  .. 
       ..

zblock ..
        ..
-}

-- Shapes



tBlock :: Square
tBlock = [square,(translate 25 0 square),(translate 50 0 square),(translate 25 (-25) square)]

iBlock :: Square
iBlock = [square,(translate 25 0 square),(translate 50 0 square), translate 75 0 square]

oBlock :: Square
oBlock = [square,(translate 25 0 square),(translate 25 25 square),(translate 0 25 square)]

jBlock :: Square
jBlock = [square,(translate 25 0 square),(translate 25 25 square),(translate 25 50 square)]

lBlock :: Square
lBlock = [square,(translate 25 0 square),(translate 0 25 square),(translate 0 50 square)]

sBlock :: Square
sBlock = [square,(translate 25 0 square),(translate 25 25 square), (translate 50 25 square)]

zBlock :: Square
zBlock = [(translate 0 25 square),(translate 25 25 square),(translate 25 0 square),(translate 50 0 square)]





























{-
tBlock = Line [
  (0,300),
  (300,300),
  (300,200),
  (200,200),
  (200,100),
  (100,100),
  (100,200),
  (0,200),
  (0,300)
   ]


longBlock = Line [
  (0,300),
  (400,300),
  (400,200),
  (0,200),
  (0,300)
  ]

-}

          


window = InWindow "Tetris" (400,400) (10,10)
