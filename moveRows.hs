import Graphics.Gloss

type GridSquare = (Bool, Color)
type Field = [[GridSquare]]
type FieldRow = [GridSquare]

initialField :: Field
initialField = take 20 (repeat (take 10 (repeat (False,black))))

moveRows :: Field -> Field
moveRows field = addRow (rowsMissing 20 (clearRows field)) (clearRows field)
  where
    -- Checks how many rows are missing
    rowsMissing :: Int -> Field -> Int
    rowsMissing n [] = n
    rowsMissing n (x:xs) = rowsMissing (n-1) xs
    -- Adds n amount of rows 
    addRow :: Int -> Field -> Field
    addRow (-1) _ = []
    addRow n field = addRow (n-1) [(take 5 (repeat (False,black)))] ++ field

-- Deletes all full rows
clearRows :: Field -> Field
clearRows [] = []
clearRows (x:xs) | row == x = clearRows xs 
                 | otherwise = x : clearRows xs
                 where
                   row = (isCleared 0 (x:xs))

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


test4 = [[(False,black),(False,black),(False,black),(False,black),(False,black)],
  [(False,black),(False,black),(False,black),(False,black),(False,black)],
         [(True,black),(True,black),(True,black),(True,black),(True,black)],
         [(True,black),(True,black),(True,black),(False,black),(True,black)],
         [(False,black),(False,black),(False,black),(False,black),(False,black)]]

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


stringify :: Field -> IO () 
stringify (x:[]) = putStrLn (stringify' x) 
stringify (x:xs) = do
  putStrLn (stringify' x)
  stringify xs

stringify' :: [GridSquare] -> String
stringify' [] = []
stringify' ((cell,color):xs) | cell = "x " ++ stringify' xs
                             | otherwise = "_ " ++ stringify' xs

