import Graphics.Gloss

type GridSquare = (Bool, Color)
type Field = [[GridSquare]]
type FieldRow = [GridSquare]


-- Returnerar en lista med y koordinaterna på raderna som är fulla och därför ska rensas
isCleared :: Int -> Field -> [Int]
isCleared _ [] = []
isCleared yc (x:xs) | isCleared' x = (yc+1) : (isCleared (yc+1) xs) 
                    | otherwise = isCleared (yc+1) xs
                    where
                      -- Kollar om en rad är full
                      isCleared' :: FieldRow -> Bool
                      isCleared' [] = True
                      isCleared' (x:xs) | (fst(x)) = isCleared' xs
                                        | otherwise = False

-- isCleared 0 test1 == [2,3]
test1 = [[(False,black),(False,black),(False,black),(False,black),(False,black)],
         [(True,black),(True,black),(True,black),(True,black),(True,black)],
         [(True,black),(True,black),(True,black),(True,black),(True,black)],
         [(False,black),(False,black),(False,black),(False,black),(False,black)]]

-- isCleared 0 test2 == [1,2]
test2 = [[(True,black),(True,black),(True,black),(True,black),(True,black)],
         [(True,black),(True,black),(True,black),(True,black),(True,black)]]

-- isCleared 0 test3 == []
test3 = [[(True,black),(False,black),(True,black),(True,black),(True,black)],
         [(True,black),(True,black),(False,black),(True,black),(True,black)]]

-- isCleared 0 initialField == []

initialField :: Field
initialField = take 20 (repeat (take 10 (repeat (False,black))))
