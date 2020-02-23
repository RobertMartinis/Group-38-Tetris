import Graphics.Gloss

type GridSquare = (Bool, Color)
type Field = [[GridSquare]]
type FieldRow = [GridSquare]

-- findIndex fungerar nog inte pga att värdet ändras

findTail :: [[a]] -> [a]
findTail [x] = x
findTail (x:xs) = findTail xs

-- Example: clearRows (isCleared 0 test1) test1

--test :: Field -> Field
--test

-- Clears the row with the corresponding y-coordinate
clearRows :: Field -> Field
clearRows [] = []
clearRows (x:xs) | row == x = clearRow x : clearRows xs
                 | otherwise = x : clearRows xs
                  where
                               -- Empties a row (Changes all true values to False)
                    clearRow :: FieldRow -> FieldRow
                    clearRow [] = []
                    clearRow ((bool,color):xs) = (False, black) : clearRow xs
                      -- Finds the index of a given element
                    findIndex :: Int -> FieldRow -> Field -> Int
                    findIndex index elem (x:xs) | x == elem = index
                                                | otherwise = findIndex (index+1) elem xs
                    row = (isCleared 0 (x:xs))

testt1 = [(True,black),(True,red),(True,black),(True,black),(True,black)]

-- Returnerar raden som är full och ska rensas
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

-- clearRows 0 test1 == [2,3]
test1 = [[(False,black),(False,black),(False,black),(False,black),(False,black)],
         [(True,black),(True,black),(True,black),(True,black),(True,black)],
         [(True,black),(True,black),(True,black),(False,black),(True,black)],
         [(False,black),(False,black),(False,black),(False,black),(False,black)]]

-- isCleared 0 test2 == [1,2]
test2 = [[(True,black),(True,black),(True,black),(True,black),(True,black)],
         [(True,black),(True,black),(True,black),(True,black),(True,black)]]

-- isCleared 0 test3 == []
test3 = [[(True,black),(False,black),(True,black),(True,black),(True,black)],
         [(True,black),(True,black),(True,black),(True,black),(True,black)]]

-- isCleared 0 initialField == []

initialField :: Field
initialField = take 20 (repeat (take 10 (repeat (False,black))))
