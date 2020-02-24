import Graphics.Gloss


type GridSquare = (Bool, Color)
type Field = [[GridSquare]]


gameOver :: Field -> Bool
gameOver (x:xs) = gameOver' x
  where
    gameOver' :: [GridSquare] -> Bool
    gameOver' [] = False
    gameOver' ((bool,_):xs) | bool == True = True
                            | otherwise = gameOver' xs
-- GameOver test1 == False
test1 = [[(False,black),(False,black),(False,black),(False,black),(False,black)],
         [(False,black),(True,black),(True,black),(False,black),(False,black)]]

-- GameOver test2 == True
test2 = [[(True,black),(False,black),(False,black),(False,black),(False,black)],
         [(False,black),(True,black),(True,black),(False,black),(False,black)]]

-- GameOver initialField == False

initialField :: [[GridSquare]]
initialField = take 20 (repeat (take 10 (repeat (False,black))))
