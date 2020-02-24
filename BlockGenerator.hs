import System.Random -- För o skapa randomBlocks


randBlock = randBlock' [tBlock,iBlock,oBlock,jBlock,lBlock,sBlock,zBlock]
  where
    randBlock' :: [a] -> IO a
    randBlock' list = do
      r <- randomRIO (0, length list - 1)
      return $ list !! r

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
